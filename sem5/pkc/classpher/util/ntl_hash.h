#pragma once
#include <NTL/ZZ.h>
#include <sstream>
#include <string>

namespace std {
template <>
struct hash<NTL::ZZ> {
  size_t operator()(const NTL::ZZ& x) const {
    std::stringstream s;
    s << x;
    return hash<string>()(s.str());
  }
};

}  // namespace std
