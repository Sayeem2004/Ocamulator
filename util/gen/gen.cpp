#include <bits/stdc++.h>
using namespace std;

int main() {
    ios::sync_with_stdio(0);
    cin.tie(0); cout.tie(0);

    string line;
    vector<string> res;

    while( getline(cin, line) ) {
        stringstream a(line);

        for (int i = 0; i < 17; i++) {
            string dum; a >> dum;

            if (i != 0 and dum != "") {
                res.push_back( "0x" + dum );
            }
        }
    }

    for (int i = 0; i < res.size(); i++) {
        cout << res[i] << ", ";
    }
    cout << "\n";
}
