#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <sys/ioctl.h>

void print_mac_addr ()
{
  int skfd;
  struct ifreq ifr ;

  skfd = socket(AF_INET, SOCK_DGRAM, 0);
  if (skfd >= 0)
    {
      strcpy(ifr.ifr_name, "eth0");
      if (ioctl(skfd, SIOCGIFHWADDR, &ifr) >= 0) {
         printf ("HW-Addr: %02x:%02x:%02x:%02x:%02x:%02x\n",
                ifr.ifr_hwaddr.sa_data[0], ifr.ifr_hwaddr.sa_data[1],
                ifr.ifr_hwaddr.sa_data[2], ifr.ifr_hwaddr.sa_data[3],
                ifr.ifr_hwaddr.sa_data[4], ifr.ifr_hwaddr.sa_data[5]);
      }
      close (skfd);
    }
}

int main ()
{
  print_mac_addr ();

  return 0;
}
