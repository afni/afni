/*balloon.c - Apply the balloon haemodynamic model of Buxton et al. to
  produce a model haemodynamic time series from a time series of stimuli or
  behavioural responses.
  Copyright (c) 2005 Matthew Belmonte

  Reference:
	RB Buxton, EC Wong, LR Frank. Dynamics of blood flow and
	oxygenation changes during brain activation: the balloon
	model. Magnetic Resonance in Medicine 39(6):855-864 (1998).

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  If you find this program useful, please send mail to Matthew Belmonte
  <belmonte@mit.edu>.
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_odeiv.h>

/*mean venous transit time at rest (seconds) - value from page 858, column 2
  of Buxton & al. 1998*/
#define tau_0 2.0

/*resting oxygen extraction fraction - from the legend of Figure 2 (page 859)
  of Buxton & al. 1998*/
#define E_0 0.4

/*resting blood volume - from the legend of Figure 2 (page 859) of Buxton & al.
  1998*/
#define V_0 0.01

/*maximum normalised inflow rate - must be greater than 1 -
  this value read from the "Flow" panel of Figure 2 (page 859)
  of Buxton & al. 1998*/
#define f_max 1.7

double t_rise,		/*inflow rise time (seconds) - must be non-negative*/
	t_sustain,	/*inflow sustain time (seconds) - must be non-negative*/
	t_fall;		/*inflow fall time (seconds) - must be non-negative*/

/*outflow exponent - from the legend of Figure 2 (page 859)
  of Buxton & al. 1998*/
#define f_alpha 0.5

/*outflow linear coefficient - from the constraint f_out(1.3) = 1.7
  in the "Fout(V)" panel of Figure 2 (page 859) of Buxton & al. 1998*/
#define f_beta 1.866

/*initial step size for Runge-Kutta-Fehlberg numerical integration*/
#define RKF_STEP 1e-6

/*normalised inflow rate - a function of time*/
double f_in(t)
double t;
  {
  if(t <= 0.0) return(1.0);
  if(t < t_rise) return(1.0 + t*(f_max-1.0)/t_rise);
  if(t <= t_rise+t_sustain) return(f_max);
  if(t <= t_rise+t_sustain+t_fall)
    return(f_max - (t-t_rise-t_sustain)*(f_max-1.0)/t_fall);
  return(1.0);
  }

/*normalised outflow rate - a function of normalised volume -
  page 857, column 2 of Buxton & al. 1998: "we modeled f_out(v)
  as a sum of a linear component and a power law"*/
double f_out(v)
double v;
  {
  return(pow(v, f_alpha) + f_beta*(v-1.0));
  }

/*oxygen extraction fraction - a function of (in)flow -
  Equation 6 (page 857) of Buxton & al. 1998*/
double E(f)
double f;
  {
  return(1.0 - pow(1.0-E_0, 1.0/f));
  }

/*change in normalised blood volume over time - Equation 5 (page 857)
  of Buxton & al. 1998*/
int compute_dvdt(t, v, dvdt, params)
double t;
const double *v;
double *dvdt;
void *params;
  {
  *dvdt = (f_in(t) - f_out(*v))/tau_0;
  return(GSL_SUCCESS);
  }

/*compute the 1x1 Jacobian comprising (d/dv)(dv/dt), and also the second
  derivative d2v/dt2.*/
int compute_v_jacobian(t, v, ddv_dvdt, d2vdt2, params)
double t;
const double *v;
double *ddv_dvdt, *d2vdt2;
void *params;
  {
  *ddv_dvdt = (-1.0/tau_0)*(f_alpha*pow(*v, f_alpha-1.0) + f_beta);
  if((t < 0.0)
   ||((t >= t_rise) && (t <= t_rise+t_sustain))
   ||(t > t_rise + t_sustain + t_fall))
    *d2vdt2 = 0.0;
  else
    {
    if(t < t_rise)
      *d2vdt2 = (f_max-1.0)/t_rise;
    else /* t_rise+t_sustain < t <= t_rise+t_sustain+t_fall*/
      *d2vdt2 = (1.0-f_max)/t_fall;
    }
  return(GSL_SUCCESS);
  }

static double ln_1_minus_E0,		/*constant used in d2qdt2()*/
	v_t0,				/*time point used in v(t)*/
	v_dt,				/*time step used in v(t)*/
	vval,				/*current v used in v(t)*/
	q_t0,				/*time point used in q(t)*/
	q_dt,				/*time step used in q(t)*/
	qval;				/*current q used in q(t)*/
static gsl_odeiv_step *v_step, *q_step;
static gsl_odeiv_control *v_ctrl, *q_ctrl;
static gsl_odeiv_evolve *v_evlv, *q_evlv;
static gsl_odeiv_system v_sys, q_sys;

/*normalised blood volume as a function of time - computed by numerical
  solution of the differential equation specified by compute_dvdt() above*/
double v(t)
double t;
  {
  while((v_t0 < t)
      &&(gsl_odeiv_evolve_apply(v_evlv, v_ctrl, v_step, &v_sys,
				&v_t0, t, &v_dt, &vval) == GSL_SUCCESS))
      ;
  return(vval);
  }

/*change in normalised deoxyhaemoglobin concentration over time - Equation 5
  (page 857) of Buxton & al. 1998*/
int compute_dqdt(t, q, dqdt, params)
double t;
const double *q;
double *dqdt;
void *params;
  {
  double ft,			/* f_in(t) */
	vt;			/* v(t) */
  ft = f_in(t);
  vt = v(t);
  *dqdt = (ft*E(ft)/E_0 - f_out(vt)*(*q)/vt)/tau_0;
  return(GSL_SUCCESS);
  }

/*compute the 1x1 Jacobian comprising (d/dq)(dq/dt), and also the second
  derivative d2q/dt2.*/
int compute_q_jacobian(t, q, ddq_dqdt, d2qdt2, params)
double t;
const double *q;
double *ddq_dqdt, *d2qdt2;
void *params;
  {
  double vt;			/* v(t) */
  vt = v(t);
  *ddq_dqdt = -f_out(vt)/(tau_0*vt);
  if((t < 0.0)
   ||((t >= t_rise) && (t <= t_rise+t_sustain))
   ||(t > t_rise + t_sustain + t_fall))
    *d2qdt2 = 0.0;
  else
    {
    double ft,			/* f_in(t) */
	   ex;			/* (1-E_0)^(1/f_in(t)) */
    ft = f_in(t);
    ex = pow(1.0-E_0, 1.0/ft);
    if(t < t_rise)
      *d2qdt2 = ((f_max-1.0)*(1.0-ex) + (ex*(f_max-1.0)*ln_1_minus_E0)/ft)
		/ (t_rise * E_0 * tau_0);
    else /* t_rise+t_sustain < t <= t_rise+t_sustain+t_fall*/
      *d2qdt2 = ((f_max-1.0)*(1.0-ex) - ex*(f_max-1.0)*ln_1_minus_E0/ft)
		/ (t_fall * E_0 * tau_0);
    }
  return(GSL_SUCCESS);
  }

/*normalised deoxyhaemoglobin concentration as a function of time - computed by
  numerical solution of the differential equation specified by compute_dqdt()
  above*/
double q(t)
double t;
  {
  while((q_t0 < t)
      &&(gsl_odeiv_evolve_apply(q_evlv, q_ctrl, q_step, &q_sys,
				&q_t0, t, &q_dt, &qval) == GSL_SUCCESS))
      ;
  return(qval);
  }

/*modelled blood-oxygen-level-dependent signal as a function of time -
  in Figure 2 (page 859) of Buxton & al. 1998, percent signal increase is
  0.9 for a deoxyhaemoglobin concentration decrease of 0.15, and the legend
  states that V0 is 0.01.  The legend for Figure 1 (page 858) states that the
  amplitude of percent signal change "scales directly with the blood volume V0."
  So we invert the deoxyhaemoglobin effect by subtracting it from 1, and then
  apply the 0.9/0.15 ratio from Figure 2, scaled by the ratio of our V_0 to
  Figure 2's V_0 of 0.01.*/
double BOLD(t)
double t;
  {
  return(V_0/0.01 * (0.9/0.15) * (1.0-q(t)));
  }

/*must be called before the first call to v() or q()*/
void create_balloon()
  {
  ln_1_minus_E0 = log(1.0-E_0);
  v_t0 = -RKF_STEP;			/*initial time*/
  v_dt = RKF_STEP;			/*initial time step*/
  vval = 1.0;				/*since normalised volume v(0) = 1*/
  q_t0 = -RKF_STEP;			/*initial time*/
  q_dt = RKF_STEP;			/*initial time step*/
  qval = 1.0;				/*since normalised q(0) = 1*/
  v_step = gsl_odeiv_step_alloc(gsl_odeiv_step_rk8pd, 1);
  q_step = gsl_odeiv_step_alloc(gsl_odeiv_step_rk8pd, 1);
  v_ctrl = gsl_odeiv_control_y_new(RKF_STEP, 0.0);
  q_ctrl = gsl_odeiv_control_y_new(RKF_STEP, 0.0);
  v_evlv = gsl_odeiv_evolve_alloc(1);
  q_evlv = gsl_odeiv_evolve_alloc(1);
  v_sys.function = compute_dvdt;
  v_sys.jacobian = compute_v_jacobian;
  v_sys.dimension = 1;
  v_sys.params = (void *)0;
  q_sys.function = compute_dqdt;
  q_sys.jacobian = compute_q_jacobian;
  q_sys.dimension = 1;
  q_sys.params = (void *)0;
  }

/*should be called after the last call to v() or q()*/
void destroy_balloon()
  {
  gsl_odeiv_evolve_free(q_evlv);
  gsl_odeiv_evolve_free(v_evlv);
  gsl_odeiv_control_free(q_ctrl);
  gsl_odeiv_control_free(v_ctrl);
  gsl_odeiv_step_free(q_step);
  gsl_odeiv_step_free(v_step);
  }

typedef struct {double delta;	/*ceil(EVENT_TIME / TR) - (EVENT_TIME / TR)*/
	int ceiling;} event;	/*ceil(EVENT_TIME / TR)*/

/*return -1 if t1->delta < t2->delta, 0 if t1->delta = t2->delta, or
  1 if t1->delta > t2->delta*/
int evt_compar(t1, t2)
const void *t1, *t2;
  {
  double diff;
  diff = ((event *)t1)->delta - ((event *)t2)->delta;
  return((diff < 0.0)? -1: (diff == 0.0)? 0: 1);
  }

int main(argc, argv)
int argc;
char **argv;
  {
  double t,
	TR,			/*scan repetition time*/
	*model,			/*output BOLD time series*/
	(*envelope)[3]=NULL;/*rise, sustain, and fall times for events*/
  event *evt;			/*event times read from evtfile*/
  int num_acq,			/*total number of scans*/
	num_evt;		/*number of haemodynamic events*/
  FILE *evtfile;		/*file containing event times*/
  register int read_params_from_data_file,	/*flag*/
	i, j;
  fprintf(stderr, "References (please cite both):\n\n"

"THEORETICAL MODEL:\n"
"RB Buxton, EC Wong, LR Frank. Dynamics of blood flow and oxygenation changes\n"
"during brain activation: the balloon model. Magnetic Resonance in Medicine\n"
"39(6):855-864 (1998).\n\n"

"PRACTICAL IMPLEMENTATION:\n"
"MK Belmonte. In preparation - for updated reference contact belmonte@mit.edu\n"
    );
  if((argc < 4)
   || ((TR = atof(argv[1])) < 0.0)
   || ((num_acq = atoi(argv[2])) <= 0)
   || ((argc > 4)
     &&  ((argc != 7)
       || ((t_rise = atof(argv[4])) < 0.0)
       || ((t_sustain = atof(argv[5])) < 0.0)
       || ((t_fall = atof(argv[6])) < 0.0)
       || (t_rise + t_sustain + t_fall == 0.0))))
    {
    fprintf(stderr,
"\nUSAGE: %s TR N event_times [ t_rise t_sustain t_fall ]\n"
"TR: scan repetition time in seconds\n"
	"\t(the output curve will be sampled at this interval)\n"
"N: number of scans (the output curve will comprise this number of samples)\n"
"event_times: The name of a file containing the event timings, in seconds, as\n"
	"\tASCII strings separated by white space, with time 0 being the time\n"
	"\tat which the initial scan occurred.\n\n"
"t_rise: haemodynamic rise time in seconds (typically between 4s and 6s)\n"
"t_sustain: haemodynamic sustain in seconds (typically between 0s and 4s)\n"
"t_fall: haemodynamic fall time in seconds (typically between 4s and 6s)\n"
	"\tIf t_rise, t_sustain, and t_fall aren't specified on the command\n"
	"\tline, then the program will expect to find event-related values of\n"
	"\tthese parameters to the right of each entry in the event file,\n"
	"\tseparated by spaces: in this case each line of the event file must\n"
	"\tcontain exactly four numbers - the event time, the haemodynamic\n"
	"\trise time for this event, the haemodynamic sustain time for this\n"
	"\tevent, and the haemodynamic fall time for this event.  (These\n"
	"\tevent-related values could for example be made to depend on a\n"
	"\tbehavioural variable such as reaction time.)\n",
      *argv);
    if( argc < 4 ) exit(0);  /* status 0 on -help    18 Sep 2018 [rickr] */
    else           exit(1);
    }
  evtfile = fopen(argv[3], "r");
  if(evtfile == NULL)
    {
    perror(argv[3]);
    exit(errno);
    }
  num_evt = 0;
  while(fscanf(evtfile, "%lf", &t) == 1)
    num_evt++;
  read_params_from_data_file = (argc == 4);
  if(read_params_from_data_file)
    {
    if(num_evt % 4 != 0)
      {
      fprintf(stderr,
	"%s: bad format, expected lines of four floating-point values each\n",
	argv[3]);
      fclose(evtfile);
      exit(1);
      }
    num_evt /= 4;		/*four values per event*/
    envelope = (void *)calloc(num_evt, sizeof(*envelope));
    if(envelope == NULL)
      {
      perror((char *)0);
      fclose(evtfile);
      exit(errno);
      }
    }
  evt = (event *)calloc(num_evt, sizeof(*evt));
  if(evt == (event *)0)
    {
    perror((char *)0);
    if(read_params_from_data_file)
      free(envelope);
    fclose(evtfile);
    exit(errno);
    }
  rewind(evtfile);
  for(i = 0; i != num_evt; i++)
    {
    if(((!read_params_from_data_file) && (fscanf(evtfile, "%lf", &t) != 1))
    || (read_params_from_data_file && ((fscanf(evtfile, "%lf %lf %lf %lf", &t, envelope[i], envelope[i]+1, envelope[i]+2) != 4))))
      {
      fclose(evtfile);
      fprintf(stderr,
	"Error: event file %s changed as it was being read.\n", argv[3]);
      exit(1);
      }
    if(read_params_from_data_file)     /*envelope parameters can't be negative*/
      for(j = 0; j != 3; j++)
	if(envelope[i][j] < 0.0)
	  envelope[i][j] = 0.0;
    t /= TR;			/*use units of TR's instead of seconds*/
    evt[i].delta = ceil(t);
    evt[i].ceiling = (int)(evt[i].delta);
    evt[i].delta -= t;
    if((t < 0.0) || (evt[i].ceiling >= num_acq))
      {
      num_evt--;
      i--;
      fprintf(stderr,
	"Event at %.3fs is outside scan time bounds 0s..%.3fs. Ignored.\n",
	t, (num_acq-1)*TR);
      }
    }
  fclose(evtfile);
  model = (double *)calloc(num_acq, sizeof(*model));
/*no need to initialise model[] to 0, since calloc() accomplishes this for us
  (assuming IEEE or some other floating-point representation in which zero is
  represented by bytes that all are zero)*/
  if(model == (double *)0)
    {
    perror((char *)0);
    exit(errno);
    }
  if(read_params_from_data_file)
    {
  /*Because the data file specifies distinct heamodynamic parameters for each
    event, we instantiate the balloon model separately for each event, by
    copying the rise, sustain, and fall times from envelope[i] and computing
    the balloon model over the entire time evolution of the event:*/
  /*inv: For all events evt[0..i-1], BOLD responses modelled with parameters
    envelope[0..i-1], respectively, have been summed into model[evt[i].ceiling
    .. num_acq-1].*/
    for(i = 0; i != num_evt; i++)
      {
      t_rise = envelope[i][0];
      t_sustain = envelope[i][1];
      t_fall = envelope[i][2];
      create_balloon();
    /*inv: the BOLD response for event time evt[i] at scan acquisitions
      evt[i].ceiling .. evt[i].ceiling+j-1 has been summed into
      model[evt[i].ceiling .. evt[i].ceiling+j-1].*/
      for(j = 0; evt[i].ceiling+j < num_acq; j++)
	model[evt[i].ceiling+j] += BOLD((j+evt[i].delta)*TR);
      destroy_balloon();
      }
    }
  else
    {
  /*If, on the other hand, the haemodynamic parameters are the same for
    every event, we can economise by arranging the event times in order of
    nondecreasing delta - that is, so that the events that occur closest to the
    following scan acquisition will be processed first - and then instantiating
    the balloon model only once.  (The numerical integrator is most efficient
    when BOLD(t) is evaluated in order of nondecreasing t.)*/
    qsort(evt, num_evt, sizeof(*evt), evt_compar);
  /*inv: For every event time evt[i], the BOLD responses for the scan
    acquisitions numbered evt[i].ceiling .. evt[i].ceiling+j-1 have been summed
    into model[evt[i].ceiling .. min{evt[i].ceiling+j-1, num_acq-1}].*/
    j = 0;
    create_balloon();
    do
      {
    /*inv: For event times evt[0..i-1], the BOLD response for the
      scan acquisition numbered evt[i].ceiling+j has been summed into
      model[evt[i].ceiling+j] if evt[i].ceiling+j < num_acq.*/
      for(i = 0; i != num_evt; i++)
        if(evt[i].ceiling+j < num_acq)
      /*evt[i].delta is the fraction of a TR from the time of the event to the
        time of the first scan acquisition ator after the time of the event, and
        j is the number of scan acquisitions, at or after the time of the event,
        for which the haemodynamic response has already been computed.*/
	  model[evt[i].ceiling+j] += BOLD((j+evt[i].delta)*TR);
      }while(++j != num_acq);
    destroy_balloon();
    }
  free(evt);
/*output the computed model*/
  for(i = 0; i != num_acq; i++)
    printf("%f\n", model[i]);
  free(model);
  return 0;
  }
