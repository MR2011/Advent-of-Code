#include <vector>
#include <iostream>
#include <queue>
#include <tuple>
#include <set>
using namespace std;
using ll = long long;
using lll = tuple<ll,ll,ll>;
using llll = tuple<ll,ll,ll,ll>;

bool is_valid(ll typ, ll tool) {
  if(typ == 0 && (tool==0 || tool==1)) { return true; }
  if(typ == 1 && (tool==1 || tool==2)) { return true; }
  if(typ == 2 && (tool==0 || tool==2)) { return true; }
  return false;
}

int main() {
  ll R = 2000;
  ll C = 1000;
  ll depth = 6969;
  ll TR = 796;
  ll TC = 9;

  /*
  ll R = 20;
  ll C = 20;
  ll TR = 10;
  ll TC = 10;
  ll depth = 510;*/

  ll MOD = 20183;
  vector<vector<ll>> G(R, vector<ll>(C, 0));
  vector<vector<ll>> E(R, vector<ll>(C, 0));
  for(ll r=0; r<R; r++) {
    for(ll c=0; c<C; c++) {
      if(r==0 && c==0) { G[r][c]=0; }
      else if(r==TR && c==TC) { G[r][c]=0; }
      else if(r==0) { G[r][c] = c*16807; }
      else if(c==0) { G[r][c] = r*48271; }
      else { G[r][c] = E[r-1][c]*E[r][c-1]; }
      E[r][c] = (G[r][c]+depth)%MOD;
    }
  }
  ll risk = 0;
  for(ll r=0; r<=TR; r++) {
    for(ll c=0; c<=TC; c++) {
      risk += (E[r][c]%3);
    }
  }
  cout << risk << endl;

  vector<ll> DR{-1,0,1,0};
  vector<ll> DC{0,1,0,-1};
  set<lll> SEEN;
  priority_queue<llll> Q;
  // torch, climbing, neither
  Q.push(make_tuple(0, 0, 0, 0));
  while(!Q.empty()) {
    ll d, r, c, tool;
    std::tie(d,r,c,tool) = Q.top(); Q.pop();
    d = -d;
    cout << d << " " << r << " " << c << " " << tool << " " << Q.size() << endl;
    if(r==TR && c==TC && tool==0) {
      cout << d << endl;
      break;
    }
    if(SEEN.count(make_tuple(r,c,tool))==1) { continue; }
    SEEN.insert(make_tuple(r,c,tool));


    for(ll tt=0; tt<3; tt++) {
      if(is_valid(E[r][c]%3, tt)) {
        Q.push(make_tuple(-(d+7), r, c, tt));
      }
    }
    for(ll dd=0; dd<4; dd++) {
      ll rr = r+DR[dd];
      ll cc = c+DC[dd];
      if(!(0<=rr&&rr<R && 0<=cc&&cc<C)) { continue; }
      if(is_valid(E[rr][cc]%3, tool)) { 
        Q.push(make_tuple(-(d+1), rr, cc, tool));
      }
    }
  }
}
