#include <bits/stdc++.h>
#define nmax 32
using namespace std;

int n;
double t, sol;



void solve(int question, double prob_so_far) {
    // analyzes options and makes a decision
    // when faced with the question'th question
    // and some probability of reaching this stage.

    if(question == n) {
        sol += prob_so_far * int(1<<n);
        return;
    }

    cout<<"question = "<<question<<", prob_so_far = "<<fixed<<setprecision(3)<<prob_so_far<<"\n";

    if(t >= 0.5) {
        // risking the answer is always preferable in this case, so there is no
        // current expected anything; I just go further with the question and
        // return the future winnings.

        double p = (1.0 + t) / 2.0;
        solve(question+1, prob_so_far * p);
    }
    else {
        // some of the time (when p < 0.5) I settle for what I have.
        // some of the time (when p > 0.5) I risk answering.
        // both situations occur with their respective probabilities and
        // expected winnings.

        double p_settle = (0.5 - t) / (1.0 - t);
        int settle_winnings = 1 << question;

        double p_risk = 0.5 / (1.0 - t);
        int risk_winnings = 1 << (question+1);

        cout<<fixed<<setprecision(3);
        cout<<"p_settle = "<<p_settle<<"\n";
        cout<<"p_risk = "<<p_risk<<"\n";

        cout<<"settle_winnings = "<<settle_winnings<<"\n";
        cout<<"risk_winnings = "<<risk_winnings<<"\n";

        double new_winnings = prob_so_far * (p_settle * settle_winnings); //p_risk * risk_winnings * 0.75);
        cout<<"add to the solution: "<<new_winnings<<"\n";
        sol += new_winnings;

        solve(question+1, prob_so_far * p_risk * (0.5 + 1.0) / 2.0);
    }
}

int main() {
    while(true) {
        cin>>n>>t;

        if(n == 0)
            return 0;


        sol = 0.0;
        solve(0, 1.0);

        cout<<"sol = "<<sol<<"\n\n";
    }

    return 0;
}
