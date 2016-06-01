#include <bits/stdc++.h>
#include "linkedlist.h"
using namespace std;

int main() {
    srand(time(0));
    std::ios::sync_with_stdio(false);

    linkedlist <pair <int, int>> l;
    linkedlist <int> arrows;
    int n, x, y, r, last_arrow = 0;

    cin>>n;
    for(int i=1; i<=n; i++) {
        cin>>x>>y>>r;
        l.push_back(make_pair(x-r, x+r));
    }

    /* Idea:
     *
     * Map each circle to an interval [x-r, x+r] (that is,
     * it's projection on the x-axis).
     *
     * Process the intervals from left to right on the x-axis.
     * Whenever the right end of an interval is reached,
     * do the following:
     *     - shoot an arrow at that coordinate,
     *     - ignore all intervals that overlap at that coordinate.
     *
     * Example:
     *
     * --------                            (1)
     *    ---------                        (2)
     *     ---                             (3)
     *       ^  --------------             (4)
     *       |     --------                (5)
     *       |            ^     ----       (6)
     *       |            |        ^
     *       |            |        |
     * 12345678901234567890123456789
     *
     * The first right-end belongs to interval (3).
     * Shoot an arrow at x = 7.
     * Ignore intervals (1) (2) and (3) in the future.
     *
     * The next right-end belongs to interval (5).
     * Shoot an arrow at x = 20.
     * Ignore intervals (5) and (6) in the future.
     *
     * The last right-end belongs to interval (6).
     * Shoot an arrow at x = 29.
     * Ignore interval (6) in the future.
     */

    l.bubble_sort();
    l.go_to_beginning();

    while(true) {
        pair <int, int> balloon = l.get_current_data();

        if(last_arrow < balloon.first || balloon.second < last_arrow) {
            last_arrow = balloon.second;
            arrows.push_back(last_arrow);
        }

        if(!l.has_next())
            break;
        l.go_next();
    }

    cout<<arrows.size()<<" arrows needed:\n";
    for(auto it = arrows.front()->_next; it != arrows.back(); it=it->_next)
        cout<<it->_data<<" ";
    cout<<"\n";

    l.clear();
    arrows.clear();

    return 0;
}
