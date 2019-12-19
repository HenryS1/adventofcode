#include <set>
#include <queue>
#include <map>
#include <vector>
#include <fstream>
#include <ctype.h>

using namespace std;

struct coord {
  int row, col;
  coord(int row, int col): row(row), col(col) {}
};

struct location {
  coord crd; char c;
  location(coord crd, char c): crd(crd), c(c) {}
};

struct visit {
  coord crd; int dist;
  visit(coord crd, int dist): crd(crd), dist(dist) {}
};

struct edge {
  coord start, end;
  int cost;
  edge(coord start, coord end, int cost): start(start), end(end), cost(cost) {}
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
  fstream f(file);
  string line;
  while (getline(f, line)) {
    result.push_back(line);
  }
  return result;
}

bool open_square(char c) {
  return c == '.' || isalnum(c);
}

void find_neighbours(queue<visit>& q, const visit v, const vector<string>& mp, set<coord>& seen) {
  coord c = v.crd;
  if (c.row > 0 && open_square(mp[c.row - 1][c.col]) && seen.find(coord(c.row - 1, c.col)) == seen.end()) {
    seen.emplace(c.row - 1, c.col);
    q.push({{c.row - 1, c.col}, v.dist + 1});
  }
  if (c.col > 0 && open_square(mp[c.row][c.col - 1]) && seen.find(coord(c.row, c.col - 1)) == seen.end()) {
    seen.emplace(c.row, c.col - 1);
    q.push({{c.row, c.col - 1}, v.dist + 1});
  }
  if (c.row < mp.size() - 1 && open_square(mp[c.row + 1][c.col])
      && seen.find(coord(c.row + 1, c.col)) == seen.end()) {
    seen.emplace(c.row + 1, c.col);
    q.push({{c.row + 1, c.col}, v.dist + 1});
  }
  if (c.col < mp.size() - 1 && open_square(mp[c.row][c.col + 1])
      && seen.find(coord(c.row, c.col + 1)) == seen.end()) {
    seen.emplace(c.row, c.col + 1);
    q.push({{c.row, c.col + 1}, v.dist + 1});
  }
}

vector<edge> edges_from(coord start, const vector<string>& mp) {
  queue<visit> q;
  set<coord> seen;
  q.push({start, 0});
  vector<edge> edges;
  while (!q.empty()) {
    visit v = q.front();
    q.pop();
    char c = mp[v.crd.row][v.crd.col];
    if (isalnum(c)) {
      edge e(start, v.crd, v.dist);
      edges.push_back(e);
    }
    if (c == '.') {
      find_neighbours(q, v, mp, seen);
    }
  }
  return edges;
}

vector<location> find_locations(coord start, const vector<string>& mp) {
  vector<location> locations;
  for (int r = 0; r < mp.size(); r++) {
    for (int c = 0; c < mp.size(); c++) {
      char ch = mp[r][c];
      if (isalnum(c)) {
        location l({r, c}, ch);
        locations.push_back(l);
      }
    }
  }
  return locations;
}

int main() {}
