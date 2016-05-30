#pragma once
#include <iostream>

template <class T>
class node {
public:
    T _data;
    node <T> *_prev;
    node <T> *_next;

    node() {
        _prev = nullptr;
        _next = nullptr;
    }
};

template <class T>
class linkedlist {
private:
    node <T> *_front;
    node <T> *_back;
    int _size;

public:
    linkedlist() {
        _size = 0;
        _front = new node <T>;
        _back = new node <T>;

        _front->_next = _back;
        _back->_prev = _front;

    }

    ~linkedlist() {
        clear();
        delete _front;
        delete _back;
    }

    bool empty() {
        return _size == 0;
    }

    int size() {
        return _size;
    }

    node <T> *front() {  // ??
        return _front;
    }

    node <T> *back() {
        return _back;
    }

    void push_back(const T &x) {
        node <T> *last = _back->_prev;
        node <T> *t = new node <T>;
        t->_data = x;

        last->_next = t;
        t->_prev = last;
        t->_next = _back;
        _back->_prev = t;

        ++_size;
    }

    void push_front(const T &x) {
        node <T> *first = _front->_next;
        node <T> *t = new node <T>;
        t->_data = x;

        _front->_next = t;
        t->_prev = _front;
        t->_next = first;
        first->_prev = t;

        ++_size;
    }

    void pop_back() {
        node <T> *last = _back->_prev;
        last->_prev->_next = _back;
        _back->_prev = last->_prev;

        delete last;
    }

    void pop_front() {
        node <T> *first = _front->_next;
        _front->_next->_prev = _front;
        _front->_next = first->_next;

        delete first;
    }

    void clear() {
        node <T> *it = _front->_next;

        while(it->_next != nullptr) {
            it = it->_next;
            delete it->_prev;
        }
        _front->_next = _back;
        _back->_prev = _front;
    }

    void bubble_sort() {
        bool sorted = false;

        while(!sorted) {
            sorted = true;

            for(auto it = _front->_next; it->_next != _back; ) {
                if(it->_data > it->_next->_data) {
                    sorted = false;

                    node <T> *a = it->_prev;
                    node <T> *b = it;
                    node <T> *c = it->_next;
                    node <T> *d = it->_next->_next;

                    // a -> b -> c -> d
                    // should become
                    // a -> c -> b -> d

                    a->_next = c;
                    c->_prev = a;
                    c->_next = b;
                    b->_prev = c;
                    b->_next = d;
                    d->_prev = b;
                }
                else
                    it = it->_next;
            }
        }
    }

    void show() {
        for(auto it = _front->_next; it != _back; it=it->_next)
            //std::cout<<it->_data.first<<" "<<it->_data.second<<"\n";
            std::cout<<it->_data<<" ";
        std::cout<<"\n";
    }
};
