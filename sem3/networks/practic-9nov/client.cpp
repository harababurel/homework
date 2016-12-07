#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <bits/stdc++.h>
using namespace std;

/* #define server_name "localhost" */
//#define server_ip "172.30.0.4"
#define server_ip "172.30.118.43"
#define port 8125
#define filename "asdf"

struct message {
    int piece_no;
    char c;
};

int main(int argc, char *argv[]) {
   int n;
   char buff[1024];
   sockaddr_in server, from;

   int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
   if (sockfd < 0)
       return 1;

    server.sin_family = AF_INET;
    server.sin_port = htons(port);
    server.sin_addr.s_addr = INADDR_ANY;//inet_addr(server_ip);
    server.sin_addr.s_addr = inet_addr(server_ip);
    memset(&(server.sin_zero), '\0', 8);

    const int length=sizeof(struct sockaddr_in);
    socklen_t length2;

    std::ifstream input(filename, std::ios::binary);
    //copies all data into buffer
    std::vector<char> buffer((std::istreambuf_iterator<char>(input)), (std::istreambuf_iterator<char>()));

    int index = 0;
    for(auto x:buffer) {
        cout<<"Sending piece no. "<<index<<": "<<x<<"\n";

        message t;
        t.piece_no = index++;
        t.c = x;

        while(true) {
            // simulate unordered packet flow by using random indices
            t.piece_no = rand()%17;

            n = sendto(sockfd, &t, sizeof(t), 0, (struct sockaddr*)(&server), length);
            /* n = send(sockfd, &piece, sizeof(piece), 0); */
            if (n < 0)
                perror("Error sending piece");

            int ack;
            n = recvfrom(sockfd, &ack, sizeof(ack), 0, (struct sockaddr*)(&from), &length2);
            if (n < 0)
                perror("recvfrom error");

            cout<<"Got an ack: "<<ack<<"\n";

            if(ack == -1)
                break;
        }
    }
    return 0;
}
