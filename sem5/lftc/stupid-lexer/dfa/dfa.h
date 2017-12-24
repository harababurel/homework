#pragma once
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include "json.hpp"

namespace lftc {
using json = nlohmann::json;

class DeterministicFiniteAutomaton {
 public:
  struct State {
    std::string name;
    std::unordered_map<std::string, std::string> edges;
    bool initial;
    bool terminal;

    std::string Next(const std::string& symbol) {
      if (edges.find(symbol) != edges.end()) {
        return edges[symbol];
      }
      return "";
    }
  };

 public:
  DeterministicFiniteAutomaton(json body) {
    /* body_ = json::parse(json_encoding); */
    body_ = body;

    for (json::iterator x = body_.begin(); x != body_.end(); x++) {
      std::string state_name = x.key();

      auto& state_properties = x.value();
      /* std::cout << "state name: " << state_name */
      /*           << "; properties: " << state_properties << "\n\n"; */

      bool initial =
          state_properties.find("initial") != state_properties.end() &&
          state_properties["initial"];

      bool terminal =
          state_properties.find("final") != state_properties.end() &&
          state_properties["final"];

      std::unordered_map<std::string, std::string> edges;

      for (json::iterator edge = state_properties["edges"].begin();
           edge != state_properties["edges"].end(); edge++) {
        std::string value = edge.value();
        /* std::cout << "key: " << edge.key() << ", value: " << edge.value() */
        /*           << "\n"; */

        edges[edge.key()] = edge.value();
      }

      states_[state_name] = State{state_name, edges, initial, terminal};
    }

    SetInitialState();
  }

  State* get_state(const std::string& name) {
    if (states_.find(name) != states_.end()) {
      return &(states_[name]);
    }
    return nullptr;
  }

  std::string LongestAcceptedPrefix(const std::string& sequence) {
    State* state = initial_state_;
    int best_i = -1;

    for (int i = 0; i < int(sequence.size()); i++) {
      state = get_state(state->Next(sequence.substr(i, 1)));

      if (state == nullptr) {
        break;
      }

      if (state->terminal) {
        best_i = i;
      }
    }

    return sequence.substr(0, 1 + best_i);
  }

  bool Accepts(const std::string& sequence) {
    return LongestAcceptedPrefix(sequence) == sequence;
  }

 private:
  void SetInitialState() {
    int initial_state_count = 0;
    for (auto& entry : states_) {
      State& state = entry.second;
      if (state.initial) {
        initial_state_count++;
        initial_state_ = &state;
      }
    }

    if (initial_state_count != 1) {
      std::cout << "There are " << initial_state_count << " initial states.\n";
      throw;
    }
  }

  json body_;
  std::unordered_map<std::string, State> states_;
  State* initial_state_ = nullptr;
};

}  // namespace lftc
