.. _center:

********************
**When and how to center a variable?**
********************

.. _Major_points:


Major points
------------

* Centering is crucial for interpretation when group effects are of interest.

* Centering is not necessary if only the covariate effect is of interest.

* Centering (and sometimes standardization as well) could be important for the numerical schemes to converge.

* Centering does not have to be at the mean, and can be any value within the range of the covariate values.

* When multiple groups of subjects are involved, centering becomes
  more complicated. Sometimes overall centering makes sense. However,
  in contrast to the popular misconception in the field, under some
  circumstances within-group centering can be meaningful (and even
  crucial) and may avoid the following problems with overall or
  grand-mean centering: 

  #. loss of the integrity of group comparisons;
  
  #. multi-collinearity; 
  
  #. invalid extrapolation of linearity.


* When multiple groups of subjects are involved, it is recommended
  that the interactions between groups and the quantitative covariate
  be modeled unless prior information exists otherwise

* To avoid unnecessary complications and misspecifications,
  categorical variables, regardless of interest or not, are better
  modeled directly as factors instead of user-defined variables
  through dummy coding as typically seen in the field. In doing so,
  centering can be automatically taken care of by the program without
  any potential mishandling, and potential interactions would be
  properly considered.

Usage clarifications of "covariate"
-----------------------------------

There are three usages of the word covariate commonly seen in the
literature, and they cause some unnecessary confusions. Originally the
word was adopted in the 1940s to connote a variable of quantitative
nature (e.g., age, IQ) in ANCOVA, replacing the phrase concomitant
variable by R. A. Fisher. Such usage has been extended from the ANCOVA
context, and sometimes refers to a variable of no interest
(extraneous, confounding or nuisance variable) to the investigator
(e.g., sex, handedness, scanner). These subtle differences in usage
stem from designs where the effects of interest are experimentally
manipulable while the effects of no interest are usually difficult to
control or even intractable. Occasionally the word covariate means any
explanatory variable among others in the model that co-account for
data variability. Mathematically these differences do not matter from
the modeling perspective. Here we use quantitative covariate (in
contrast to its qualitative counterpart, factor) instead of covariate
to avoid confusion.

Purpose of modeling a quantitative covariate
--------------------------------------------

Ideally all samples, trials or subjects, in an FMRI experiment are
drawn from a completely randomized pool in terms of BOLD response,
cognition, or other factors that may have effects on BOLD
response. However, such randomness is not always practically
guaranteed or achievable. In many situations (e.g., patient
recruitment) the investigator does not have a set of homogeneous
subjects, and the potentially unaccounted variability sources in
cognitive capability or BOLD response could distort the analysis if
handled improperly, and may lead to compromised statistical power,
inaccurate effect estimates, or even inferential failure. For example,
direct control of variability due to subject performance (e.g.,
response time in each trial) or subject characteristics (e.g., age,
IQ, brain volume, psychological features, etc.) is most likely
unrealistic. Instead, indirect control through statistical means may
become crucial, achieved by incorporating one or more concomitant
measures in addition to the variables of primary interest. Such
concomitant variables or covariates, when incorporated in the model,
might provide adjustments to the effect estimate, and increase
statistical power by accounting for data variability some of which
cannot be explained by other explanatory variables than the
covariate. Such adjustment is loosely described in the literature as a
process of “regressing out”, “partialling out”, “controlling for” or
“correcting for” the variability due to the covariate
effect. Typically, a covariate is supposed to have some cause-effect
relation with the outcome variable, the BOLD response in the case of
FMRI data. Potential covariates include age, personality traits, and
behavioral data. They are sometime of direct interest (e.g.,
personality traits), and other times are not (e.g., age). They are
mostly continuous (or quantitative) variables; however, discrete
(qualitative or categorical) variables are occasionally treated as
covariates in the literature (e.g., sex) if they are not specifically
of interest except to be "regressed" out in the analysis.

While stimulus trial-level variability (e.g., reaction time) is
usually modeled through amplitude or parametric modulation in single
subject analysis, the covariates typically seen in the brain imaging
group analysis are task-, condition-level or subject-specific measures
such as age, IQ, psychological measures, and brain volumes, or
behavioral data at condition- or task-type level. Although amplitude
modulation accounts for the trial-to-trial variability, for example,
with linear or quadratic fitting of some behavioral measures that
accounts for habituation or attenuation, the average value of such
behavioral measure from each subject still fluctuates across
subjects. Therefore it may still be of importance to run group
analysis with the average measure from each subject as a covariate at
group level.

Incorporating a quantitative covariate in a model at the group level
may serve two purposes, increasing statistical power by accounting for
data variability and estimating the magnitude (and significance) of
the confounding effect. However, two modeling issues deserve more
attention in practice, covariate centering and its interactions with
other effects, due to their consequences on result interpretability
and inferences. And these two issues are a source of frequent
inquiries, confusions, model misspecifications and misinterpretations
across analysis platforms, and not even limited to neuroimaging
community. Centering a covariate is crucial for interpretation if
inference on group effect is of interest, but is not if only the
covariate effect is of interest. And in contrast to the popular
conception, centering does not have to hinge around the mean, and can
be any value that is meaningful and when linearity holds. This is the
reason we prefer the generic term "centering" instead of the popular
description "demeaning" or "mean-centering" in the field.

Centering with one group of subjects
------------------------------------

Two parameters in a linear system are of potential research interest,
the intercept and the slope. The former reveals the group mean effect
when the covariate is at the value of zero, and the slope shows the
covariate effect accounting for the subject variability in the
covariate. In other words, the slope is the marginal (or differential)
effect of the covariate, the amount of change in the response variable
when the covariate increases by one unit. For example, in the case of
IQ as a covariate, the slope shows the average amount of BOLD response
change when the IQ score of a subject increases by one. Depending on
the specific scenario, either the intercept or the slope, or both, are
of interest to the investigator. However, one would not be interested
in the group or population effect with an IQ of 0. Instead the
investigator would more likely want to estimate the average effect at
the sample mean (e.g., 104.7) of the subject IQ scores or the
population mean (e.g., 100). If the group average effect is of
research interest, a practical technique, centering, not usually
highlighted in formal discussions, becomes crucial because the effect
corresponding to the covariate at the raw value of zero is not
necessarily interpretable or interesting.

Centering typically is performed around the mean value from the
sampled subjects, and such a convention was originated from and
confounded by regression analysis and ANOVA/ANCOVA framework in which
sums of squared deviation relative to the mean (and sums of products)
are computed. In most cases the average value of the covariate is a
valid estimate for an underlying or hypothetical population, providing
a pivotal point for substantive interpretation. However, the centering
value does not have to be the mean of the covariate, and should be
based on the expediency in interpretation.  Suppose the IQ mean in a
group of 20 subjects is 104.7. By subtracting each subject’s IQ score
by 104.7, one provides the centered IQ value in the model (1), and the
estimate of intercept α0 is the group average effect corresponding to
the group mean IQ of 104.7. On the other hand, suppose that the group
of 20 subjects recruited from a college town has an IQ mean of 115.0,
which is not well aligned with the population mean, 100. Through the
manual transformation of centering (subtracting the raw covariate
values by the center), one may analyze the data with centering on the
population mean instead of the group mean so that one can make
inferences about the whole population, assuming the linear fit of IQ
holds reasonably well within the typical IQ range in the
population. Another example is that one may center the covariate with
the same value as a previous study so that cross-study comparison can
be achieved. Similarly, centering around a fixed value other than the
mean is typically seen in growth curve modeling for longitudinal
studies (Biesanz et al., 2004) in which the average time in one
experiment is usually not generalizable to others. For instance, in a
study of child development (Shaw et al., 2006) the inferences on the
correlation between cortical thickness and IQ required that centering
of the age be around, not the mean, but each integer within a sampled
age range (from 8 up to 18). In general, centering artificially shifts
the values of a covariate by a value that is of specific interest
(e.g., IQ of 100) to the investigator so that the new intercept
corresponds to the effect when the covariate is at the center
value. In other words, by offsetting the covariate to a center value c
the x-axis shift transforms the effect corresponding to the covariate
at c to a new intercept in a new system.

In addition to the distribution assumption (usually Gaussian) of the
residuals (e.g., di in the model (1)), the following two assumptions
are typically mentioned in traditional analysis with a covariate
(e.g., ANCOVA): exact measurement of the covariate, and linearity
between the covariate and the dependent variable. Regarding the first
assumption, the explanatory variables in a regression model such as
(1) should be idealized predictors (e.g., presumed hemodynamic
response function), or they have been measured exactly and/or observed
without error. This assumption is unlikely to be valid in behavioral
data, and significant unaccounted-for estimation errors in the
covariates can lead to inconsistent results and potential
underestimation of the association between the covariate and the
response variable—the attenuation bias or regression dilution (Greene,
2003). In regard to the linearity assumption, the linear fit of the
covariate effect may predict well for a subject within the covariate
range, but does not necessarily hold if extrapolated beyond the range
that the sampled subjects represent as extrapolation is not always
reliable or even meaningful. The assumption of linearity in the
traditional ANCOVA framework is due to the limitations in modeling
interactions in general, as we will see more such limitations
later. Nonlinearity, although unwieldy to handle, are not necessarily
prohibitive, if there are enough data to fit the model adequately. And
nonlinear relationships become trivial in the context of general
linear model (GLM), and, for example, quadratic or polynomial
relationship can be interpreted as self-interaction.

To reiterate the case of modeling a covariate with one group of
subjects, the inclusion of a covariate is usually motivated by the
more accurate group effect (or adjusted effect) estimate and improved
power than the unadjusted group mean and the corresponding
significance testing obtained through the conventional one-sample
Student's t-test. Centering the covariate may be essential in
interpreting the group effect (or intercept) while controlling for the
variability in the covariate, and it is unnecessary only if the
covariate effect (or slope) is of interest in the simple regression
model. The center value can be the sample mean of the covariate or any
other value of interest in the context.

Centering with more than one group of subjects
----------------------------------------------

When multiple groups are involved, four scenarios exist regarding
centering and interaction across the groups: same center and same
slope; same center with different slope; same slope with different
center; and different center and different slope. None of the four
scenarios is prohibited in modeling as long as a meaningful hypothesis
can be framed. However, presuming the same slope across groups could
be problematic unless strong prior knowledge exists. We suggest that
researchers report their centering strategy and justifications of
interaction modeling or the lack thereof. Extra caution should be
exercised if a categorical variable is considered as an effect of no
interest because of its coding complications on interpretation and the
consequence from potential model misspecifications.

Is within-group centering meaningful?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When more than one group of subjects are involved, even though
within-group centering is generally considered inappropriate (e.g.,
Poldrack et al., 2011), it not only can improve interpretability under
some circumstances, but also can reduce collinearity that may occur
when the groups differ significantly in group average. More
specifically, within-group centering makes it possible in one model

A. to compare the group difference while accounting for within-group
   age differences, and at the same time, and

#. to examine the age effect and its interaction with the groups.

If the groups differ significantly regarding the quantitative
covariate, cross-group centering may encounter three issues:
collinearity between the subject-grouping variable and the
quantitative covariate, invalid extrapolation of linearity to the
overall mean where little data are available, and loss of the
integrity of group comparison. Not only may centering around the
overall mean nullify the effect of interest (group difference), but it
could also lead to either uninterpretable or unintended results such
as Lord’s paradox (Lord, 1967; Lord, 1969). In contrast, within-group
centering, even though rarely performed, offers a unique modeling
strategy that should be seriously considered when appropriate (e.g.,
`Chen et al., 2014 <https://afni.nimh.nih.gov/pub/dist/HBM2014/Chen_in_press.pdf>`_). [CASLC_2014]_

When to center within- or across-groups?
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Suppose that one wants to compare the response difference between the
two sexes to face relative to building images. Other than the
conventional two-sample Student's t-test, the investigator may
consider the age (or IQ) effect in the analysis even though the two
groups of subjects were roughly matched up in age (or IQ) distribution
when they were recruited. Further suppose that the average ages from
the two sexes are 36.2 and 35.3, very close to the overall mean age of
35.7. One may center all subjects’ ages around the overall mean of
35.7 or (for comparison purpose) an average age of 35.0 from a
previous study. However, one extra complication here than the case
with one group of subject discussed in the previous section is that
the investigator has to decide whether to model the sexes with the
same of different age effect (slope). However, unless one has prior
knowledge of same age effect across the two sexes, it would make more
sense to adopt a model with different slopes, and, if the interaction
between age and sex turns out to be statistically insignificant, one
may tune up the original model by dropping the interaction term and
reduce to a model with same slope.

However, if the age (or IQ) distribution is substantially different
across the two sexes, systematic bias in age exists across the two
groups; that is, age as a variable is highly confounded (or highly
correlated) with the grouping variable. One may face an unresolvable
challenge in including age (or IQ) as a covariate in analysis. For
instance, suppose the average age is 22.4 years old for males and 57.8
for females, and the overall mean is 40.1 years old. Even without
explicitly considering the age effect in analysis, a two-sample
Student t-test is problematic because sex difference, if significant,
might be partially or even totally attributed to the effect of age
difference, leading to a compromised or spurious inference. If one
includes age as a covariate in the model through centering around a
constant or overall mean, one wants to “control” or “correct” for the
age variability across all subjects in the two groups, but the risk is
that, with few or no subjects in either or both groups around the
center value (or, overall average age of 40.1 years old), inferences
on individual group effects and group difference based on
extrapolation are not reliable as the linearity assumption about the
age effect may break down. Another issue with a common center for the
covariate is that the inference on group difference may partially be
an artifact of measurement errors in the covariate (Keppel and
Wickens, 2004).  On the other hand, one may model the age effect by
centering around each group’s respective constant or mean. Even though
the age effect is controlled within each group and the risk of
within-group linearity breakdown is not severe, the difficulty now
lies in the same result interpretability as the corresponding
two-sample Student t-test: the sex difference may be compounded with
the effect of age difference across the groups.

In the above example of two groups with different covariate
distribution, age (or IQ) strongly correlates with the grouping
variable, and it violates an assumption in conventional ANCOVA, the
covariate is independent of the subject-grouping variable. Regardless
the centering options (different or same), covariate modeling has been
discouraged or strongly criticized in the literature (e.g., Neter et
al., 1996; Miller and Chapman, 2001; Keppel and Wickens, 2004;
Sheskin, 2004). The moral here is that this kind of modeling
difficulty is due to imprudent design in subject recruitment, and can
and should be prevented. If a subject-related variable might have
impact on the experiment, the variable distribution should be kept
approximately the same across groups when recruiting subjects.

A different situation from the above scenario of modeling difficulty
is the following, which is not formally covered in literature. Suppose
that one wishes to compare two groups of subjects, adolescents and
seniors, with their ages ranging from 10 to 19 in the adolescent group
and from 65 to 100 in the senior group. Again age (or IQ) is strongly
correlated with the grouping variable, and violates the assumption in
conventional ANCOVA, the covariate is independent of the
subject-grouping factor. Although not a desirable analysis, one might
center all subjects’ ages around a constant or overall mean and ask
the following trivial or even uninteresting question: would the two
groups differ in BOLD response if adolescents and seniors were no
different in age (e.g., centering around the overall mean of age for
all subjects, for instance, 43.7 years old)? In addition to the
interpretation difficulty, when the common center value is beyond the
covariate range of each group, the linearity does not necessarily hold
well when extrapolated to a region where the covariate has no or only
few data points available. A third issue surrounding a common center
is that the inference on group difference may partially be an artifact
of measurement errors in the covariate (Keppel and Wickens,
2004). However, what is essentially different from the previous
example is that the problem in this case lies in posing a sensible
question in the substantive context, but not in modeling with a
covariate per se that is correlated with a subject-grouping factor in
general. In addition, the independence assumption in the conventional
ANCOVA is not needed in this case. More specifically, we can
reasonably test whether the two groups have the same BOLD response
while controlling for the within-group variability in age. When the
groups differ significantly on the within-group mean of a covariate,
the model could be formulated and interpreted in terms of the effect
on the response variable relative to what is expected from the
difference across the groups on their respective covariate centers
(controlling for within-group variability), not if the two groups had
no difference in the covariate (controlling for variability across all
subjects). That is, if the covariate values of each group are offset
by the within-group center (mean or a specific value of the covariate
for that group), one can compare the effect difference between the two
subpopulations, assuming that the two groups have same or different
age effect. Again unless prior information is available, a model with
different age effect between the two groups (Fig. 2D) is more
favorable as a starting point.

We have discussed two examples involving multiple groups, and both
examples consider age effect, but one includes sex groups while the
other has young and old. The common thread between the two examples is
that the covariate distribution is substantially different across
groups, and the subject-specific values of the covariate is highly
confounded with another effect (group) in the model. However, unlike
the situation in the former example, the age distribution difference
in the two groups of young and old is not attributed to a poor design,
but to the intrinsic nature of subject grouping. Such an intrinsic
difference of covariate distribution across groups is not rare. A
similar example is the comparison between children with autism and
ones with normal development while IQ is considered as a
covariate. Again comparing the average effect between the two groups
if they had the same IQ is not particularly appealing. Instead one is
usually interested in the group contrast when each group is centered
around the within-group IQ center while controlling for the
within-group IQ effects. A third case is to compare a group of
subjects who are averse to risks and those who seek risks (Neter et
al., 1996). The risk-seeking group is usually younger (20 - 40 years
old) than the risk-averse group (50 – 70 years old). As Neter et
al. (1996) argued, comparing the two groups at the overall mean (e.g.,
45 years old) is inappropriate and hard to interpret, and therefore
they discouraged considering age as a controlling variable in the
analysis. However, it is not unreasonable to control for age
variability within each group and center each group around a
meaningful age (e.g. group mean). A fourth scenario is reaction time
or anxiety rating as a covariate in comparing the control group and an
anxiety group where the groups have preexisting mean difference in the
covariate values. All these examples show that proper centering not
only improves interpretability and allows for testing meaningful
hypotheses, but also may help in resolving the confusions and
controversies surrounding some unnecessary assumptions about covariate
modeling.

Categorical variables as regressors of no interest
--------------------------------------------------

It is not rarely seen in literature that a categorical variable such
as sex, scanner, or handedness is “partialled” or “regressed” out as a
covariate (in the usage of regressor of no interest). Since such a
variable is dummy-coded with quantitative values, caution should be
taken in centering, because it would have consequences in the
interpretation of other effects. Furthermore, if the effect of such a
variable is included in the model, examining first its effect and
potential interactions with effects of interest might be necessary,
regardless whether such an effect – and its interaction with other
fixed effects – is of scientific interest. Such a strategy warrants a
detailed discussion because of its consequences in interpreting other
effects. That is, when one discusses an overall mean effect with a
grouping factor (e.g., sex) as an explanatory variable, it is
implicitly assumed that interactions or varying average effects occur
across groups. Were the average effect the same across all groups, one
would model the effects without having to specify which groups are
averaged over, and the grouping factor would not be considered in the
first place. The interactions usually shed light on the
generalizability of main effects because the interpretation of the
main effects may be affected or tempered by the presence of a
significant interaction (Keppel and Wickens, 2004; Moore et al., 2004;
Chow, 2003; Cabrera and McDougall, 2002; Muller and Fetterman,
2002). Simple partialling without considering potential main effects
and/or interactions may distort the estimation and significance
testing for the effects of interest, and merely including a grouping
factor as additive effects of no interest without even an attempt to
discuss the group differences or to model the potential interactions
invites for potential misinterpretation or misleading conclusions.

We do not recommend that a grouping variable be modeled as a simple
additive effect for two reasons: the influence of group difference on
interpreting other effects, and the risk of model misspecification in
the presence of interactions with other effects. All possible
interactions with other effects (continuous or categorical variables)
should be considered unless they are statistically insignificant or
can be ignored based on prior knowledge. When an overall effect across
groups is desirable, one needs to pay attention to centering when
adopting a coding strategy, and effect coding is favorable for its
immunity to unequal number of subjects across groups. However, such
overall effect is not generally appealing: if group differences exist,
they deserve more deliberations, and the overall effect may be
difficult to interpret in the presence of group differences or with
the existence of interactions between groups and other effects; if
group differences are not significant, the grouping variable can be
dropped through model tuning. Overall, we suggest that a categorical
variable (regardless of interest or not) be treated a typical
factor. In doing so, one would be able to avoid the complications of
dummy coding and the associated centering issues.

So far we have only considered such fixed effects of a continuous
variable as well as a categorical variable that separates subjects
into multiple groups. Historically ANCOVA was the merging fruit of
ANOVA and regression, and we have seen the limitations imposed on the
traditional ANCOVA framework. Naturally the GLM provides a further
integration beyond ANCOVA. It is worth mentioning that another
assumption about the traditional ANCOVA with two or more groups is the
homogeneity of variances, same variability across groups. However, it
is challenging to model heteroscedasticity, different variances across
groups, even under the GLM scheme. Furthermore, of note in the case of
a subject-grouping (or between-subjects) factor is that all its levels
are independent with each other. When the effects from a
within-subject (or repeated-measures) factor are involved, the GLM
approach becomes cumbersome. Furthermore, a model with random slope is
not possible within the GLM framework. These limitations necessitate
the extension of GLM and lead to the multivariate modeling (MVM) (Chen
et al., 2013) and linear mixed-effect (LME) modeling (Chen et al.,
2014) so that the cross-levels correlations of such a factor and
random slopes can be properly modeled.

References
----------

Chen, G., Adleman, N.E., Saad, Z.S., Leibenluft, E., Cox, R.W. (2014). 
Applications of Multivariate Modeling to Neuroimaging Group Analysis: A
Comprehensive Alternative to Univariate General Linear Model. NeuroImage 99,
571-588. 10.1016/j.neuroimage.2014.06.027
https://afni.nimh.nih.gov/pub/dist/HBM2014/Chen_in_press.pdf

Poldrack, R.A., Mumford, J.A., Nichols, T.E., 2011. Handbook of
Functional MRI Data Analysis. Cambridge University Press.

