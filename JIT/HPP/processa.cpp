#include <iostream>
#include <fstream>
#include <string>
#include <string.h>
#include <cstdlib>

using namespace std;

#define NFILES 13

string files[NFILES] =   {"yaam_primitive_predicates",
                          "yaam_call",
						  "yaam_call_count",
						  "yaam_cut",
						  "yaam_failure",
						  "yaam_get",
						  "yaam_indexing_ext",
						  "yaam_indexing_std",
						  "yaam_misc",
						  "yaam_pop",
						  "yaam_put",
						  "yaam_unify",
						  "yaam_write"};

int main(int argc, char **argv)
{
    std::string fileinname, fileoutname, linein, lineout;
    char lineout_char[256];
    ifstream filein;
    ofstream fileout;
    int k;
	for (int i = 0; i < NFILES; i++)
	{
		linein = "";
		lineout = "";
		fileinname = files[i] + ".h";
		fileoutname = files[i] + "_mod.h";
		filein.open(fileinname.c_str());
		if (!filein.is_open()) {
			cout << "Erro ao abrir arquivo " << fileinname << " para leitura! Saindo..." << endl;
			exit(1);
		}
		fileout.open(fileoutname.c_str());
		if (!fileout.is_open()) {
			cout << "Erro ao abrir arquivo " << fileoutname << " para escrita! Saindo..." << endl;
			exit(1);
		}
		cout << "Processando arquivo " << fileinname << "!\n";
		getline(filein, linein);
		while (!filein.eof())
		{
		    lineout = "";
		    strcpy(lineout_char, "");
		    //cout << "linein = " << linein << endl; /* */
			if (linein.size() > 6)
			{
				if (linein.substr(0, 7) == "#define")
				{
					lineout = "#define";
					k = 0;
					for (int j = 7; j < linein.size(); j++)
					{
						if (linein[j] >= 'a' && linein[j] <= 'z')
						{
						    //cout << "linein[j] = " << linein[j] << endl;
						    lineout_char[k++] = linein[j] - 32;
						}
						else
						{
							lineout_char[k++] = linein[j];
						}
					}
					lineout_char[k] = '\0';
					lineout += lineout_char;
					//cout << "lineout 1 = " << lineout << endl;
				}
				else
				{
					lineout = linein;
					//cout << "lineout 2 = " << lineout << endl;
				}
			}
			else
			{
				lineout = linein;
				//cout << "lineout 3 = " << lineout << endl;
			}
		fileout << lineout << endl;
		getline(filein, linein);
		}
		lineout = linein;
		fileout << lineout << endl;
		filein.close();
		fileout.close();
	}
    return 0;
}
