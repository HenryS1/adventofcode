#include <queue>
#include <map>
#include <vector>
#include <priority_queue>
#include <fstream>

using namespace std;

struct coord {
  int row, col;
  coord(int row, int col): row(row), col(col) {}
};

struct edge {
  int start, end, cost;
  char c;
  edge(int start, int end, int cost, char c): start(start), end(end), cost(cost), c(c) {}
};

bool operator<(const coord& one, const coord& other) {
  return one.row < other .row || (one.row == other.row && one.col < other.col);
};

struct graph {
  map<coord, char> values;
  map<coord, edge> edges;
};

vector<string> read_map(const string& file) {
  vector<string> result;
  fstream file(file);
  string line;
  while (getline(file, line)) {
    result.push_back(line);
  }
  return result;
}

void neighbours(vector<coord>& buff, coord& c) {
  buff.clear();
  if (c.row > 0) buff.emplace_back(c.row - 1, c.col);
  if (c.col > 0) buff.emplace_back(c.row, c.col - 1);
  if (c.
}

graph make_graph(const vector<string>& input) {
  queue<int> q;
  coord start(40, 40);
  q.insert(coord);
  while (
};
