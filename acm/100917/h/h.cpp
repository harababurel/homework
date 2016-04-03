#include <bits/stdc++.h>
using namespace std;

bool date_cmp(string a, string b) {
    // 0123456789
    // 99:99:9999

    for(int i=6; i<=9; i++)
        if(a[i] != b[i])
            return a[i] < b[i];

    for(int i=3; i<=4; i++)
         if(a[i] != b[i])
            return a[i] < b[i];

    for(int i=0; i<=1; i++)
        if(a[i] != b[i])
            return a[i] < b[i];

    return true;
}



class Employee {
public:
    string name;
    int id;
    string date;
    Employee(string name, int id, string date) {
        this->name = name;
        this->id = id;
        this->date = date;
    }
    bool operator<(const Employee &rhs) const {
        if(this->date == rhs.date)
            return this->id < rhs.id;
        return date_cmp(this->date, rhs.date);
    }
};


class Department {
public:
    int id;
    int last_id;
    set <Employee> S;

    Department(int id) {
        this->id = id;
        this->last_id = 0;
    }
    bool operator<(const Department &rhs) const {
        return this->id < rhs.id;
    }
    bool operator==(const Department &rhs) const {
        return this->id == rhs.id;
    }
};

class Office {
public:
    unordered_map <int, Department*> M;
    bool has_department(int id) {
        return this->M.find(id) != this->M.end();
    }

    void add_department(int id) {

        this->M[id] = new Department(id);
    }

    void add_employee(int D, Employee emp) {
        emp.id = ++this->M[D]->last_id;
        this->M[D]->S.insert(emp);
    }

    void remove_employee(int D, int id)
        Employee temp = Employee("", id, "");
        this->M[D]->S.erase(
};


int n, t, D, k;
string name, date;


int main() {
    cin>>n;

    Office office;

    while(n--) {
        cin>>t;

        if(t==1) {
            cin>>D;
            cin>>name;
            cin>>date;

            Employee employee = Employee(name, -1, date);

            if(!office.has_department(D))
                office.add_department(D);

            office.add_employee(D, employee);

        }
        if(t==-1) {
            cin>>D;
            cin>>k;
        }
    }

    for(auto it:office.M) {
        auto dep = *it.second;
        cout<<"department "<<dep.id<<"\n";

        for(auto emp:dep.S) {
            cout<<"employee #"<<emp.id<<": "<<emp.name<<"\n";
        }
    }


    return 0;
}
