#ifndef DISTRIBUTION_H
#define DISTRIBUTION_H

#include <vector>
#include <string>

using namespace std;

class CptEntry;

class Distribution
{
  public:
    Distribution (int, double*, int, vector<string>);
    Distribution (double*, int, vector<string>);
    int                      id;
    double*                  params;
    int                      nParams;
    vector<string>           domain;
    int*                     offsets;
};

#endif // DISTRIBUTION

