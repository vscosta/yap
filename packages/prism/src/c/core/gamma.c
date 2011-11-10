/*  -*- c-basic-offset: 4 ; tab-width: 4 -*-

  This file contains a portable implementation for a couple of gamma-
  family functions, originally written for the PRISM programming system
  <http://sato-www.cs.titech.ac.jp/prism/>.

  The code is based on SPECFUN (Fortran program collection for special
  functions by W. J. Cody et al. at Argonne National Laboratory), which
  is available in public domain at <http://www.netlib.org/specfun/>.

  Here is the license terms for this file (just provided to explicitly
  state that the code can be used for any purpose):

------------------------------------------------------------------------------

  Copyright (c) 2007-2009 Yusuke Izumi

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.

------------------------------------------------------------------------------

*/

#include <math.h>
#include "core/gamma.h"

#define PI         (3.14159265358979323846) /* pi               */
#define PI_2       (1.57079632679489661923) /* pi / 2           */
#define PI_4       (0.78539816339744830962) /* pi / 4           */
#define LN_SQRT2PI (0.91893853320467274178) /* ln(sqrt(2 * pi)) */

/**
 *  Computes ln(|Gamma(x)|).
 */
double lngamma(double x)
{
    /* Constants for [0.5,1.5) -------------------------------------------*/

    const double D1 = -5.772156649015328605195174e-01;

    const double P1[] = {
        +4.945235359296727046734888e+00, +2.018112620856775083915565e+02,
        +2.290838373831346393026739e+03, +1.131967205903380828685045e+04,
        +2.855724635671635335736389e+04, +3.848496228443793359990269e+04,
        +2.637748787624195437963534e+04, +7.225813979700288197698961e+03
    };

    const double Q1[] = {
        +6.748212550303777196073036e+01, +1.113332393857199323513008e+03,
        +7.738757056935398733233834e+03, +2.763987074403340708898585e+04,
        +5.499310206226157329794414e+04, +6.161122180066002127833352e+04,
        +3.635127591501940507276287e+04, +8.785536302431013170870835e+03
    };

    /* Constants for [1.5,4.0) -------------------------------------------*/

    const double D2 = +4.227843350984671393993777e-01;

    const double P2[] = {
        +4.974607845568932035012064e+00, +5.424138599891070494101986e+02,
        +1.550693864978364947665077e+04, +1.847932904445632425417223e+05,
        +1.088204769468828767498470e+06, +3.338152967987029735917223e+06,
        +5.106661678927352456275255e+06, +3.074109054850539556250927e+06
    };

    const double Q2[] = {
        +1.830328399370592604055942e+02, +7.765049321445005871323047e+03,
        +1.331903827966074194402448e+05, +1.136705821321969608938755e+06,
        +5.267964117437946917577538e+06, +1.346701454311101692290052e+07,
        +1.782736530353274213975932e+07, +9.533095591844353613395747e+06
    };

    /* Constants for [4.0,12.0) ------------------------------------------*/

    const double D4 = +1.791759469228055000094023e+00;

    const double P4[] = {
        +1.474502166059939948905062e+04, +2.426813369486704502836312e+06,
        +1.214755574045093227939592e+08, +2.663432449630976949898078e+09,
        +2.940378956634553899906876e+10, +1.702665737765398868392998e+11,
        +4.926125793377430887588120e+11, +5.606251856223951465078242e+11
    };

    const double Q4[] = {
        +2.690530175870899333379843e+03, +6.393885654300092398984238e+05,
        +4.135599930241388052042842e+07, +1.120872109616147941376570e+09,
        +1.488613728678813811542398e+10, +1.016803586272438228077304e+11,
        +3.417476345507377132798597e+11, +4.463158187419713286462081e+11
    };

    /* Constants for [12.0,Infinity) -------------------------------------*/

    const double C[] = {
        -2.955065359477124231624146e-02, +6.410256410256410034009811e-03,
        -1.917526917526917633674555e-03, +8.417508417508417139715760e-04,
        -5.952380952380952917890600e-04, +7.936507936507936501052685e-04,
        -2.777777777777777883788657e-03, +8.333333333333332870740406e-02
    };

    /*--------------------------------------------------------------------*/

    const double EPS = 2.22e-16;
    const double P68 = 87.0 / 128.0;
    const double BIG = 2.25e+76;

    /*--------------------------------------------------------------------*/

    double  p, q, y;
    int     i, n;

    if (x != x) /* NaN */
        return x;
    else if (0 * x != 0) /* Infinity */
        return HUGE_VAL;
    else if (x <= 0.0) {
        q = modf(-2.0 * x, &p);
        n = (int)(p);
        q = sin(PI_2 * (n % 2 == 0 ? q : 1.0 - q));
        return log(PI / q) - lngamma(1.0 - x);
    }
    else if (x < EPS)
        return -log(x);
    else if (x < 0.5) {
        p = 0.0;
        q = 1.0;
        y = x;
        for (i = 0; i < 8; i++) {
            p = p * y + P1[i];
            q = q * y + Q1[i];
        }
        return x * (D1 + y * (p / q)) - log(x);
    }
    else if (x < P68) {
        p = 0.0;
        q = 1.0;
        y = x - 1.0;
        for (i = 0; i < 8; i++) {
            p = p * y + P2[i];
            q = q * y + Q2[i];
        }
        return y * (D2 + y * (p / q)) - log(x);
    }
    else if (x < 1.5) {
        p = 0.0;
        q = 1.0;
        y = x - 1.0;
        for (i = 0; i < 8; i++) {
            p = p * y + P1[i];
            q = q * y + Q1[i];
        }
        return y * (D1 + y * (p / q));
    }
    else if (x < 4.0) {
        p = 0.0;
        q = 1.0;
        y = x - 2.0;
        for (i = 0; i < 8; i++) {
            p = p * y + P2[i];
            q = q * y + Q2[i];
        }
        return y * (D2 + y * (p / q));
    }
    else if (x < 12.0) {
        p = 0.0;
        q = -1.0;
        y = x - 4.0;
        for (i = 0; i < 8; i++) {
            p = p * y + P4[i];
            q = q * y + Q4[i];
        }
        return D4 + y * (p / q);
    }
    else if (x < BIG) {
        p = 0.0;
        q = log(x);
        y = 1.0 / (x * x);
        for (i = 0; i < 8; i++) {
            p = p * y + C[i];
        }
        return p / x + LN_SQRT2PI - 0.5 * q + x * (q - 1.0);
    }
    else {
        q = log(x);
        return LN_SQRT2PI - 0.5 * q + x * (q - 1.0);
    }

    /*--------------------------------------------------------------------*/
}

/**
 *  Computes Psi(x) = (d/dx)(ln(Gamma(x)))
 */
double digamma(double x)
{
    /* Constants for [0.5,3.0] -------------------------------------------*/

    const double P1[] = {
        +4.5104681245762934160e-03, +5.4932855833000385356e+00,
        +3.7646693175929276856e+02, +7.9525490849151998065e+03,
        +7.1451595818951933210e+04, +3.0655976301987365674e+05,
        +6.3606997788964458797e+05, +5.8041312783537569993e+05,
        +1.6585695029761022321e+05
    };

    const double Q1[] = {
        +9.6141654774222358525e+01, +2.6287715790581193330e+03,
        +2.9862497022250277920e+04, +1.6206566091533671639e+05,
        +4.3487880712768329037e+05, +5.4256384537269993733e+05,
        +2.4242185002017985252e+05, +6.4155223783576225996e-08
    };

    /* Constants for (3.0,Infinity) --------------------------------------*/

    const double P2[] = {
        -2.7103228277757834192e+00, -1.5166271776896121383e+01,
        -1.9784554148719218667e+01, -8.8100958828312219821e+00,
        -1.4479614616899842986e+00, -7.3689600332394549911e-02,
        -6.5135387732718171306e-21
    };

    const double Q2[] = {
        +4.4992760373789365846e+01, +2.0240955312679931159e+02,
        +2.4736979003315290057e+02, +1.0742543875702278326e+02,
        +1.7463965060678569906e+01, +8.8427520398873480342e-01
    };

    /*--------------------------------------------------------------------*/

    const double MIN = 2.23e-308;
    const double MAX = 4.50e+015;
    const double SMALL = 5.80e-009;
    const double LARGE = 2.71e+014;

    const double X01 = 187.0 / 128.0;
    const double X02 = 6.9464496836234126266e-04;

    /*--------------------------------------------------------------------*/

    double  p, q, y, sgn;
    int     i, n;

    sgn = (x > 0.0) ? +1.0 : -1.0;

    y = fabs(x);

    if (x != x) /* NaN */
        return x;
    else if (x < -MAX || y < MIN)
        return -1.0 * sgn * HUGE_VAL;
    else if (y < SMALL)
        return digamma(1.0 - x) - 1.0 / x;
    else if (x < 0.5) {
        q = modf(4.0 * y, &p);
        n = (int)(p);

        switch (n % 4) {
        case 0:
            return digamma(1.0 - x) - sgn * PI / tan(PI_4 * q);
        case 1:
            return digamma(1.0 - x) - sgn * PI * tan(PI_4 * (1.0 - q));
        case 2:
            return digamma(1.0 - x) + sgn * PI * tan(PI_4 * q);
        case 3:
            return digamma(1.0 - x) + sgn * PI / tan(PI_4 * (1.0 - q));
        }
    }
    else if (x <= 3.0) {
        p = 0.0;
        q = 1.0;
        for (i = 0; i < 8; i++) {
            p = p * x + P1[i];
            q = q * x + Q1[i];
        }
        p = p * x + P1[8];
        return p / q * ((x - X01) - X02);
    }
    else if (x < LARGE) {
        p = 0.0;
        q = 1.0;
        y = 1.0 / (x * x);
        for (i = 0; i < 6; i++) {
            p = p * y + P2[i];
            q = q * y + Q2[i];
        }
        p = p * y + P2[6];
        return p / q - 0.5 / x + log(x);
    }

    return log(x);
}
