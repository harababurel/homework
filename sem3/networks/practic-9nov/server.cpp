#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <cstdio>
#include <bits/stdc++.h>
using namespace std;

#define port 8125

struct message {
    int piece_no;
    char c;
};

int main(int argc, char *argv[]) {
   int sock, length, n;
   struct sockaddr_in server, from;

   sock=socket(AF_INET, SOCK_DGRAM, 0);
   if (sock < 0) perror("Opening socket");
   length = sizeof(server);
   bzero(&server,length);

   server.sin_family=AF_INET;
   server.sin_addr.s_addr=INADDR_ANY;
   server.sin_port=htons(port);

   if (bind(sock,(struct sockaddr *)&server,length)<0)
       perror("binding");

   socklen_t fromlen = sizeof(struct sockaddr_in);
   message t;

   int expected_piece = 0;

   int i = 0;
   ofstream g("out.bin", ios::out | ios::binary | ios::app);

   while(1) {
       n = recvfrom(sock,&t,sizeof(t),0,(struct sockaddr *)&from,&fromlen);
       if (n < 0) perror("recvfrom");

       cout<<"Received piece no. "<<t.piece_no<<" (char = '"<<t.c<<"')\n";
       cout<<"Expected piece no. "<<expected_piece<<"\n";

       if(t.piece_no != expected_piece) {
           n = sendto(sock, &expected_piece, sizeof(expected_piece), 0, (struct sockaddr *)&from, fromlen);
       }
       else {
           int resp = -1;
           n = sendto(sock, &resp, sizeof(resp), 0, (struct sockaddr *)&from, fromlen);
           ++expected_piece;

           // write to file
           g.write (&t.c, sizeof(t.c));
           g.flush();
       }

       if (n < 0)
           perror("sendto");
       cout.flush();
   }
}
