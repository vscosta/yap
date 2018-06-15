//
// Created by vsc on 7/6/17.
//

#ifndef YAPDROID_MAIN_H
#define YAPDROID_MAIN_H
#include <cstdio>
#include <iostream>
#include <string>
#include <sstream>


struct AndroidStreamer {
    std::string *buff0;

    virtual void display(std::string text) const = 0;
    virtual ~AndroidStreamer() {}
     AndroidStreamer() {    buff0 = new std::string[256];
    };
};
void setStreamer(AndroidStreamer* streamer);
AndroidStreamer& getStreamer();

template<typename T> AndroidStreamer& operator<<(AndroidStreamer& stream, T const& val) {
    std::ostringstream s;
    s << val;
    stream.display(s.str());
    return stream;
};

#endif //YAPDROID_MAIN_H
