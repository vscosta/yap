#ifndef _UNION2_H_
#define _UNION2_H_

int unir(int *res, int rows, int tipo, int **ret, int final);

typedef struct n2
{
	int v[2];
}s2;

typedef struct n3
{
	int v[3];
}s3;

typedef struct n4
{
	int v[4];
}s4;

typedef struct n5
{
	int v[5];
}s5;

typedef struct n6
{
	int v[6];
}s6;

typedef struct n7
{
	int v[7];
}s7;

typedef struct n8
{
	int v[8];
}s8;

typedef struct n9
{
	int v[9];
}s9;

typedef struct n10
{
	int v[10];
}s10;

typedef struct n11
{
	int v[11];
}s11;

typedef struct n12
{
	int v[12];
}s12;

typedef struct n13
{
	int v[13];
}s13;

typedef struct n14
{
	int v[14];
}s14;

typedef struct n15
{
	int v[15];
}s15;

typedef struct n16
{
	int v[16];
}s16;

typedef struct n17
{
	int v[17];
}s17;

typedef struct n18
{
	int v[18];
}s18;

typedef struct n19
{
	int v[19];
}s19;

typedef struct n20
{
	int v[20];
}s20;

struct q1
{
	__host__ __device__
	bool operator()(const int &r1, const int &r2)
	{
		if(r1 != r2)
			return true;
		return false;
	}
};

struct p2
{
	__host__ __device__
	bool operator()(const s2 &r1, const s2 &r2)
	{
		int x;
		for(x = 0; x < 2; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q2
{
	__host__ __device__
	bool operator()(const s2 &r1, const s2 &r2)
	{
		int x;
		for(x = 0; x < 2; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o2
{
	__host__ __device__
	bool operator()(const s2 &r1, const s2 &r2)
	{
		int x;
		for(x = 0; x < 2; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p3
{
	__host__ __device__
	bool operator()(const s3 &r1, const s3 &r2)
	{
		int x;
		for(x = 0; x < 3; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q3
{
	__host__ __device__
	bool operator()(const s3 &r1, const s3 &r2)
	{
		int x;
		for(x = 0; x < 3; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o3
{
	__host__ __device__
	bool operator()(const s3 &r1, const s3 &r2)
	{
		int x;
		for(x = 0; x < 3; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p4
{
	__host__ __device__
	bool operator()(const s4 &r1, const s4 &r2)
	{
		int x;
		for(x = 0; x < 4; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q4
{
	__host__ __device__
	bool operator()(const s4 &r1, const s4 &r2)
	{
		int x;
		for(x = 0; x < 4; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o4
{
	__host__ __device__
	bool operator()(const s4 &r1, const s4 &r2)
	{
		int x;
		for(x = 0; x < 4; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p5
{
	__host__ __device__
	bool operator()(const s5 &r1, const s5 &r2)
	{
		int x;
		for(x = 0; x < 5; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q5
{
	__host__ __device__
	bool operator()(const s5 &r1, const s5 &r2)
	{
		int x;
		for(x = 0; x < 5; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o5
{
	__host__ __device__
	bool operator()(const s5 &r1, const s5 &r2)
	{
		int x;
		for(x = 0; x < 5; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p6
{
	__host__ __device__
	bool operator()(const s6 &r1, const s6 &r2)
	{
		int x;
		for(x = 0; x < 6; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q6
{
	__host__ __device__
	bool operator()(const s6 &r1, const s6 &r2)
	{
		int x;
		for(x = 0; x < 6; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o6
{
	__host__ __device__
	bool operator()(const s6 &r1, const s6 &r2)
	{
		int x;
		for(x = 0; x < 6; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p7
{
	__host__ __device__
	bool operator()(const s7 &r1, const s7 &r2)
	{
		int x;
		for(x = 0; x < 7; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q7
{
	__host__ __device__
	bool operator()(const s7 &r1, const s7 &r2)
	{
		int x;
		for(x = 0; x < 7; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o7
{
	__host__ __device__
	bool operator()(const s7 &r1, const s7 &r2)
	{
		int x;
		for(x = 0; x < 7; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p8
{
	__host__ __device__
	bool operator()(const s8 &r1, const s8 &r2)
	{
		int x;
		for(x = 0; x < 8; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q8
{
	__host__ __device__
	bool operator()(const s8 &r1, const s8 &r2)
	{
		int x;
		for(x = 0; x < 8; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o8
{
	__host__ __device__
	bool operator()(const s8 &r1, const s8 &r2)
	{
		int x;
		for(x = 0; x < 8; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p9
{
	__host__ __device__
	bool operator()(const s9 &r1, const s9 &r2)
	{
		int x;
		for(x = 0; x < 9; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q9
{
	__host__ __device__
	bool operator()(const s9 &r1, const s9 &r2)
	{
		int x;
		for(x = 0; x < 9; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o9
{
	__host__ __device__
	bool operator()(const s9 &r1, const s9 &r2)
	{
		int x;
		for(x = 0; x < 9; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p10
{
	__host__ __device__
	bool operator()(const s10 &r1, const s10 &r2)
	{
		int x;
		for(x = 0; x < 10; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q10
{
	__host__ __device__
	bool operator()(const s10 &r1, const s10 &r2)
	{
		int x;
		for(x = 0; x < 10; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o10
{
	__host__ __device__
	bool operator()(const s10 &r1, const s10 &r2)
	{
		int x;
		for(x = 0; x < 10; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p11
{
	__host__ __device__
	bool operator()(const s11 &r1, const s11 &r2)
	{
		int x;
		for(x = 0; x < 11; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q11
{
	__host__ __device__
	bool operator()(const s11 &r1, const s11 &r2)
	{
		int x;
		for(x = 0; x < 11; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o11
{
	__host__ __device__
	bool operator()(const s11 &r1, const s11 &r2)
	{
		int x;
		for(x = 0; x < 11; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p12
{
	__host__ __device__
	bool operator()(const s12 &r1, const s12 &r2)
	{
		int x;
		for(x = 0; x < 12; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q12
{
	__host__ __device__
	bool operator()(const s12 &r1, const s12 &r2)
	{
		int x;
		for(x = 0; x < 12; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o12
{
	__host__ __device__
	bool operator()(const s12 &r1, const s12 &r2)
	{
		int x;
		for(x = 0; x < 12; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p13
{
	__host__ __device__
	bool operator()(const s13 &r1, const s13 &r2)
	{
		int x;
		for(x = 0; x < 13; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q13
{
	__host__ __device__
	bool operator()(const s13 &r1, const s13 &r2)
	{
		int x;
		for(x = 0; x < 13; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o13
{
	__host__ __device__
	bool operator()(const s13 &r1, const s13 &r2)
	{
		int x;
		for(x = 0; x < 13; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p14
{
	__host__ __device__
	bool operator()(const s14 &r1, const s14 &r2)
	{
		int x;
		for(x = 0; x < 14; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q14
{
	__host__ __device__
	bool operator()(const s14 &r1, const s14 &r2)
	{
		int x;
		for(x = 0; x < 14; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o14
{
	__host__ __device__
	bool operator()(const s14 &r1, const s14 &r2)
	{
		int x;
		for(x = 0; x < 14; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p15
{
	__host__ __device__
	bool operator()(const s15 &r1, const s15 &r2)
	{
		int x;
		for(x = 0; x < 15; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q15
{
	__host__ __device__
	bool operator()(const s15 &r1, const s15 &r2)
	{
		int x;
		for(x = 0; x < 15; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o15
{
	__host__ __device__
	bool operator()(const s15 &r1, const s15 &r2)
	{
		int x;
		for(x = 0; x < 15; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p16
{
	__host__ __device__
	bool operator()(const s16 &r1, const s16 &r2)
	{
		int x;
		for(x = 0; x < 16; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q16
{
	__host__ __device__
	bool operator()(const s16 &r1, const s16 &r2)
	{
		int x;
		for(x = 0; x < 16; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o16
{
	__host__ __device__
	bool operator()(const s16 &r1, const s16 &r2)
	{
		int x;
		for(x = 0; x < 16; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p17
{
	__host__ __device__
	bool operator()(const s17 &r1, const s17 &r2)
	{
		int x;
		for(x = 0; x < 17; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q17
{
	__host__ __device__
	bool operator()(const s17 &r1, const s17 &r2)
	{
		int x;
		for(x = 0; x < 17; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o17
{
	__host__ __device__
	bool operator()(const s17 &r1, const s17 &r2)
	{
		int x;
		for(x = 0; x < 17; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p18
{
	__host__ __device__
	bool operator()(const s18 &r1, const s18 &r2)
	{
		int x;
		for(x = 0; x < 18; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q18
{
	__host__ __device__
	bool operator()(const s18 &r1, const s18 &r2)
	{
		int x;
		for(x = 0; x < 18; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o18
{
	__host__ __device__
	bool operator()(const s18 &r1, const s18 &r2)
	{
		int x;
		for(x = 0; x < 18; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p19
{
	__host__ __device__
	bool operator()(const s19 &r1, const s19 &r2)
	{
		int x;
		for(x = 0; x < 19; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q19
{
	__host__ __device__
	bool operator()(const s19 &r1, const s19 &r2)
	{
		int x;
		for(x = 0; x < 19; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o19
{
	__host__ __device__
	bool operator()(const s19 &r1, const s19 &r2)
	{
		int x;
		for(x = 0; x < 19; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

struct p20
{
	__host__ __device__
	bool operator()(const s20 &r1, const s20 &r2)
	{
		int x;
		for(x = 0; x < 20; x++)
		{
			if(r1.v[x] != r2.v[x])
				return false;
		}
		return true;
	}
};

struct q20
{
	__host__ __device__
	bool operator()(const s20 &r1, const s20 &r2)
	{
		int x;
		for(x = 0; x < 20; x++)
		{
			if(r1.v[x] != r2.v[x])
				return true;
		}
		return false;
	}
};

struct o20
{
	__host__ __device__
	bool operator()(const s20 &r1, const s20 &r2)
	{
		int x;
		for(x = 0; x < 20; x++)
		{
			if(r1.v[x] > r2.v[x])
				return true;
			if(r1.v[x] < r2.v[x])
				return false;
		}
		return false;
	}
};

#endif
