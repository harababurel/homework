#include <cstring>
#include <iostream>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
using namespace std;

#define MY_PORT 8080
/* #define DST_IP "86.123.1.144" */
#define DST_IP "172.30.0.4"
#define DST_PORT 8080

int main() {

    int sockfd = socket(AF_INET, SOCK_STREAM, 0);

    if(sockfd == -1) {
        cout<<"Could not create socket.\n";
        return 1;
    }
    cout<<"Socket created: fd="<<sockfd<<"\n";

    sockaddr_in my_addr;
    my_addr.sin_family = AF_INET;
    my_addr.sin_port = htons(MY_PORT);
    /* my_addr.sin_port = 0; // use random port */
    /* my_addr.sin_addr.s_addr = inet_addr("172.30.117.113"); */
    my_addr.sin_addr.s_addr = INADDR_ANY;
    memset(&(my_addr.sin_zero), '\0', 8);


    /* if(bind(sockfd, (struct sockaddr*)&my_addr, sizeof(struct sockaddr))) { */
    /*     cout<<"Could not bind to port "<<my_addr.sin_port<<"\n"; */
    /*     return 1; */
    /* } */
    /* cout<<"Binded to port "<<my_addr.sin_port<<"\n"; */

    sockaddr_in dst_addr;
    dst_addr.sin_family = AF_INET;
    dst_addr.sin_port= htons(DST_PORT);
    dst_addr.sin_addr.s_addr = inet_addr(DST_IP);
    memset(&(dst_addr.sin_zero), '\0', 8);

    if(connect(sockfd, (struct sockaddr*)&dst_addr, sizeof(dst_addr)) == -1) {
        cout<<"Could not connect to "<<DST_IP<<"\n";
        cout<<strerror(errno)<<"\n";
        perror("Error");

        return 1;
    }
    cout<<"Connected to "<<DST_IP<<"\n";

    while(1) {
        char msg[100];
        printf("Give message: ");
        fgets(msg, sizeof(msg), stdin);
        send(sockfd, &msg, sizeof(msg), 0);
        /* char resp[100]; */
        /* recv(c, &resp, sizeof(resp), 0); */
        /* printf("Received: %s\n", resp); */
    }
    /* close(sockfd); */


    return 0;
}
