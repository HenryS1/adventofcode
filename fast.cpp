#include <set>
#include <queue>
#include <map>
#include <vector>
#include <fstream>
#include <ctype.h>
#include <iostream>

using namespace std;

struct coord {
    int row, col;
    coord(int row, int col): row(row), col(col) {}
};

ostream& operator<<(ostream& os, const coord& c) {
    os << "Row " << c.row << " Col " << c.col;
    return os;
}

struct location {
    coord crd; char c;
    location(coord crd, char c): crd(crd), c(c) {}
};

ostream& operator<<(ostream& os, const location& loc) {
    os << loc.crd << " Char " << loc.c;
    return os;
}

struct visit {
    coord crd; int dist;
    visit(coord crd, int dist): crd(crd), dist(dist) {}
};

ostream& operator<<(ostream& os, const visit& v) {
    os << "Coord: " << v.crd << " Dist: " << v.dist << endl;
    return os;
}

bool operator<(const visit& one, const visit& other) {
    return one.dist < other.dist;
}

struct edge {
    coord start, end;
    int cost;
    edge(coord start, coord end, int cost): start(start), end(end), cost(cost) {}
};

bool operator<(const edge& one, const edge& other) {
    return one.cost < other.cost;
}

ostream& operator<<(ostream& os, const edge& e ) {
    os << "Start: " << e.start << " ==> End: " << e.end << " Cost: " << e.cost;
    return os;
}

bool operator<(const coord& one, const coord& other) {
    return one.row < other .row || (one.row == other.row && one.col < other.col);
}

bool operator==(const coord& one, const coord& other) {
    return one.row == other.row && one.col == other.col;
}

bool operator!=(const coord& one, const coord& other) {
    return one.row != other.row || one.col != other.col;
}

struct graph {
    map<coord, char> values;
    map<coord, vector<edge>> edges;
    graph(map<coord, char> values, map<coord, vector<edge>> edges)
        : values(move(values)), edges(move(edges)) {}
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
    return c == '.' || c == '@' || isalnum(c);
}

void find_neighbours(queue<visit>& q, const visit v, const vector<string>& mp, set<coord>& seen) {
    coord c = v.crd;
//    cout << "NEIGHBOURS OF " << v.crd << endl;
    if (c.row > 0 && open_square(mp[c.row - 1][c.col]) 
        && seen.find({c.row - 1, c.col}) == seen.end()) {
//        cout << coord(c.row - 1, c.col) << endl;
        seen.insert({c.row - 1, c.col});
        q.push({{c.row - 1, c.col}, v.dist + 1});
    }
    if (c.col > 0 && open_square(mp[c.row][c.col - 1]) 
        && seen.find({c.row, c.col - 1}) == seen.end()) {
//        cout << coord(c.row, c.col - 1) << endl;
        seen.insert({c.row, c.col - 1});
        q.push({{c.row, c.col - 1}, v.dist + 1});
    }
    if (c.row < mp.size() - 1 && open_square(mp[c.row + 1][c.col])
        && seen.find({c.row + 1, c.col}) == seen.end()) {
//        cout << coord(c.row + 1, c.col) << endl;
        seen.insert({c.row + 1, c.col});
        q.push({{c.row + 1, c.col}, v.dist + 1});
    }
    if (c.col < mp[0].size() - 1 && open_square(mp[c.row][c.col + 1])
        && seen.find({c.row, c.col + 1}) == seen.end()) {
//        cout << coord(c.row, c.col + 1) << endl;
        seen.insert({c.row, c.col + 1});
        q.push({{c.row, c.col + 1}, v.dist + 1});
    }
}

vector<edge> edges_from(coord start, const vector<string>& mp) {
    queue<visit> q;
    set<coord> seen;
    seen.insert(start);
    q.push({start, 0});
    vector<edge> edges;
    while (!q.empty()) {
        visit v = q.front();
//        cout << v << endl;
        q.pop();
        char c = mp[v.crd.row][v.crd.col];
        if (isalnum(c) && v.crd != start) {
            edge e(start, v.crd, v.dist);
            edges.push_back(e);
        }
        if (v.crd == start || (c == '.' || c == '@')) {
            find_neighbours(q, v, mp, seen);
        }
    }
    return edges;
}

vector<location> find_locations(coord start, const vector<string>& mp) {
    vector<location> locations;
    locations.push_back({start, mp[start.row][start.col]});
    for (int r = 0; r < mp.size(); r++) {
        for (int c = 0; c < mp[0].size(); c++) {
            char ch = mp[r][c];
            if (isalnum(ch)) {
                location l({r, c}, ch);
                locations.push_back(l);
            }
        }
    }
    return locations;
}

coord find_start(const vector<string>& mp) {
    for (int r = 0; r < mp.size(); r++) {
        for (int c = 0; c < mp[0].size(); c++) {
            if (mp[r][c] == '@') return coord(r, c);
        }
    }
    return coord(0, 0);
}

graph make_graph(const vector<string>& mp) {
    coord start = find_start(mp);
    vector<location> locations = find_locations(start, mp);
    map<coord, vector<edge>> graph_edges;
    for (auto loc : locations) {
//        cout << "LOCATION " << loc << endl;
        vector<edge> edges = edges_from(loc.crd, mp);
//        cout << "NUM EDGES " << edges.size() << endl;
        graph_edges.insert({loc.crd, move(edges)});
    }
    map<coord, char> values;
    for (auto loc : locations) {
        values.insert({ loc.crd, mp[loc.crd.row][loc.crd.col] });
    }
    return graph(values, graph_edges);
}

void reachable_from(coord start, 
                    vector<visit>& reachable, 
                    const set<char>& unlocked,
                    const graph& g) {
    static priority_queue<visit> pq;
    static set<coord> seen;
    seen.clear();
    seen.insert(start);
    pq.push({start, 0});
    while (!pq.empty()) {
        visit v = pq.top();
//        cout << "VISITING " << v << endl;
//        char c = g.values.find(v.crd)->second;
//        cout << "ONE" << endl;
        pq.pop();
        // if (islower(c) && seen.find(v.crd) == seen.end()) {
        //     reachable.push_back(v);
        // }
        // if (g.edges.find(v.crd) == g.edges.end()) {
        //     cout << "NO EDGES" << endl;
        // }
//        cout << "TWO" << endl;
        for (auto e : g.edges.find(v.crd)->second) {
//            cout << "EDGE " << e << endl;
            if (seen.find(e.end) == seen.end()) {
                char other_c = g.values.find(e.end)->second;
                if (islower(other_c) || unlocked.find(other_c) != unlocked.end()) {
                    visit next_v(e.end, e.cost + v.dist);
                    seen.insert(e.end);
                    pq.push(next_v);
                    if (islower(other_c) && unlocked.find(toupper(other_c)) == unlocked.end())
                        reachable.push_back(next_v);
                }
            }
        }
    }
}

void show_unlocked(const set<char>& unlocked) {
    for (auto c : unlocked) {
        cout << c << " ";
    }
    cout << endl;
}

template<typename T>
void show_path(const vector<T>& path) {
    for (auto c : path) {
        cout << c << " ";
    }
    cout << endl;
}

int best_seen = 2147483647;

int max_dist(vector<visit>& reachable) {
    int best = 0;
    for (const auto& v: reachable) {
        best = max(best, v.dist);
    }
    return best;
}

int best_number_of_moves(coord current,
                         int moves_to,
                         vector<char>& path,
                         set<char>& unlocked, 
                         const graph& g) {
//    cout << "CURRENT " << current << " " << g.values.find(current)->second << " " << moves_to << endl;
    if (unlocked.size() == 6) {
        cout << "END" << endl;
        return min(best_seen, moves_to);
    }
    vector<visit> reachable;
    reachable_from(current, reachable, unlocked, g);
//    show_path(reachable);
    if (max_dist(reachable) + moves_to >= best_seen) {
        return best_seen;
    }

    for (const auto& v : reachable) {
        char gate = toupper(g.values.find(v.crd)->second);
        unlocked.insert(gate);
        path.push_back(g.values.find(v.crd)->second);
//        cout << "DISTANCE TO " << v.dist << endl;
        int current_best = best_number_of_moves(v.crd, moves_to + v.dist, path, unlocked, g);
        if (current_best < best_seen) {
            cout << "NEW BEST " << current_best <<endl;
            best_seen = current_best;
        }
        path.pop_back();
        unlocked.erase(gate);
    }
    if (unlocked.size() < 10) {
        cout  << "NEXT TICK " << best_seen << endl;
    }
    return best_seen;
}

int main() {

    vector<string> mp = read_map("inputt");//read_map("2019/input18");

    coord start = find_start(mp);
    // cout << "START " << start << endl;

    // for (auto e : edges_from({1, 17}, mp)) {
    //     cout << e << endl;
    // }

    graph g = make_graph(mp);
    // vector<visit> reachable;
    // reachable_from({1, 17}, reachable, {'A'}, g);
    // for (auto v : reachable) {
    //     cout << v << " " << g.values.find(v.crd)->second << endl;
    // }
    set<char> unlocked;
    vector<char> path;
    int best_moves = best_number_of_moves(start, 0, path, unlocked, g);
    
    cout << "BEST MOVES " << best_moves << endl;
}
