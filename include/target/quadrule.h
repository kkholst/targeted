#ifndef GAUSSHERMITE_H
#define GAUSSHERMITE_H

#include <algorithm>
#include <RcppArmadillo.h>

class QuadRule {
private:
  arma::vec x;
  arma::vec w;
public:
  arma::vec Weight() {
    return(w);
  }
  arma::vec Abscissa() {
    return(x);
  }
  QuadRule(int n=1) :
     x(std::min(n+(n%2==0),21)),
     w(std::min(n+(n%2==0),21)) {
	switch(n) {
		case 0:
		break;
		case 1:
			x[0] = 0;
			w[0] = 1.77245385090552;
		break;
		case 2:
		case 3:
			x[0] = 1.22474487139159;
			x[1] = 8.88178419700125e-16;
			x[2] = -1.22474487139159;
			w[0] = 0.29540897515092;
			w[1] = 1.18163590060368;
			w[2] = 0.295408975150921;
		break;
		case 4:
		case 5:
			x[0] = 2.02018287045609;
			x[1] = 0.95857246461382;
			x[2] = 8.88178419700125e-16;
			x[3] = -0.958572464613816;
			x[4] = -2.02018287045608;
			w[0] = 0.0199532420590458;
			w[1] = 0.39361932315224;
			w[2] = 0.945308720482942;
			w[3] = 0.393619323152242;
			w[4] = 0.0199532420590459;
		break;
		case 6:
		case 7:
			x[0] = 2.65196135683523;
			x[1] = 1.67355162876747;
			x[2] = 0.816287882858967;
			x[3] = 1.33226762955019e-15;
			x[4] = -0.816287882858962;
			x[5] = -1.67355162876746;
			x[6] = -2.65196135683523;
			w[0] = 0.000971781245099515;
			w[1] = 0.0545155828191268;
			w[2] = 0.425607252610126;
			w[3] = 0.810264617556808;
			w[4] = 0.425607252610129;
			w[5] = 0.0545155828191272;
			w[6] = 0.000971781245099528;
		break;
		case 8:
		case 9:
			x[0] = 3.19099320178153;
			x[1] = 2.26658058453184;
			x[2] = 1.46855328921667;
			x[3] = 0.723551018752838;
			x[4] = 0;
			x[5] = -0.723551018752837;
			x[6] = -1.46855328921667;
			x[7] = -2.26658058453184;
			x[8] = -3.19099320178153;
			w[0] = 3.96069772632642e-05;
			w[1] = 0.00494362427553695;
			w[2] = 0.088474527394377;
			w[3] = 0.432651559002555;
			w[4] = 0.720235215606052;
			w[5] = 0.432651559002555;
			w[6] = 0.0884745273943762;
			w[7] = 0.00494362427553692;
			w[8] = 3.96069772632637e-05;
		break;
		case 10:
		case 11:
			x[0] = 3.66847084655958;
			x[1] = 2.78329009978165;
			x[2] = 2.02594801582575;
			x[3] = 1.32655708449493;
			x[4] = 0.6568095668821;
			x[5] = 8.88178419700125e-16;
			x[6] = -0.656809566882099;
			x[7] = -1.32655708449493;
			x[8] = -2.02594801582576;
			x[9] = -2.78329009978165;
			x[10] = -3.66847084655958;
			w[0] = 1.43956039371425e-06;
			w[1] = 0.000346819466323343;
			w[2] = 0.0119113954449115;
			w[3] = 0.117227875167709;
			w[4] = 0.429359752356122;
			w[5] = 0.654759286914593;
			w[6] = 0.429359752356126;
			w[7] = 0.117227875167708;
			w[8] = 0.0119113954449111;
			w[9] = 0.000346819466323334;
			w[10] = 1.43956039371421e-06;
		break;
		case 12:
		case 13:
			x[0] = 4.10133759617864;
			x[1] = 3.24660897837241;
			x[2] = 2.51973568567824;
			x[3] = 1.85310765160151;
			x[4] = 1.22005503659075;
			x[5] = 0.605763879171061;
			x[6] = 8.88178419700125e-16;
			x[7] = -0.605763879171056;
			x[8] = -1.22005503659075;
			x[9] = -1.85310765160151;
			x[10] = -2.51973568567824;
			x[11] = -3.24660897837241;
			x[12] = -4.10133759617864;
			w[0] = 4.82573185007318e-08;
			w[1] = 2.04303604027071e-05;
			w[2] = 0.00120745999271939;
			w[3] = 0.02086277529617;
			w[4] = 0.140323320687024;
			w[5] = 0.42161629689854;
			w[6] = 0.604393187921162;
			w[7] = 0.421616296898545;
			w[8] = 0.140323320687024;
			w[9] = 0.0208627752961696;
			w[10] = 0.00120745999271937;
			w[11] = 2.04303604027065e-05;
			w[12] = 4.82573185007304e-08;
		break;
		case 14:
		case 15:
			x[0] = 4.49999070730939;
			x[1] = 3.66995037340445;
			x[2] = 2.96716692790561;
			x[3] = 2.32573248617386;
			x[4] = 1.71999257518649;
			x[5] = 1.13611558521092;
			x[6] = 0.565069583255578;
			x[7] = 3.5527136788005e-15;
			x[8] = -0.565069583255571;
			x[9] = -1.13611558521092;
			x[10] = -1.71999257518648;
			x[11] = -2.32573248617385;
			x[12] = -2.9671669279056;
			x[13] = -3.66995037340444;
			x[14] = -4.49999070730938;
			w[0] = 1.52247580425353e-09;
			w[1] = 1.05911554771107e-06;
			w[2] = 0.0001000044412325;
			w[3] = 0.00277806884291275;
			w[4] = 0.0307800338725461;
			w[5] = 0.158488915795935;
			w[6] = 0.412028687498897;
			w[7] = 0.564100308726416;
			w[8] = 0.412028687498902;
			w[9] = 0.158488915795936;
			w[10] = 0.0307800338725457;
			w[11] = 0.00277806884291278;
			w[12] = 0.0001000044412325;
			w[13] = 1.05911554771112e-06;
			w[14] = 1.52247580425367e-09;
		break;
		case 16:
		case 17:
			x[0] = 4.8713451936744;
			x[1] = 4.06194667587547;
			x[2] = 3.37893209114149;
			x[3] = 2.75776291570389;
			x[4] = 2.17350282666662;
			x[5] = 1.61292431422123;
			x[6] = 1.06764872574345;
			x[7] = 0.531633001342657;
			x[8] = 8.88178419700125e-16;
			x[9] = -0.531633001342652;
			x[10] = -1.06764872574345;
			x[11] = -1.61292431422123;
			x[12] = -2.17350282666662;
			x[13] = -2.75776291570389;
			x[14] = -3.37893209114149;
			x[15] = -4.06194667587547;
			x[16] = -4.8713451936744;
			w[0] = 4.58057893079867e-11;
			w[1] = 4.97707898163089e-08;
			w[2] = 7.11228914002143e-06;
			w[3] = 0.000298643286697756;
			w[4] = 0.00506734995762757;
			w[5] = 0.0409200341497567;
			w[6] = 0.172648297670097;
			w[7] = 0.401826469470411;
			w[8] = 0.530917937624859;
			w[9] = 0.401826469470415;
			w[10] = 0.172648297670098;
			w[11] = 0.0409200341497562;
			w[12] = 0.00506734995762745;
			w[13] = 0.000298643286697756;
			w[14] = 7.11228914002152e-06;
			w[15] = 4.97707898163081e-08;
			w[16] = 4.58057893079847e-11;
		break;
		case 18:
		case 19:
			x[0] = 5.22027169053748;
			x[1] = 4.42853280660378;
			x[2] = 3.76218735196402;
			x[3] = 3.1578488183476;
			x[4] = 2.59113378979454;
			x[5] = 2.04923170985062;
			x[6] = 1.52417061939354;
			x[7] = 1.01036838713431;
			x[8] = 0.503520163423889;
			x[9] = 0;
			x[10] = -0.503520163423885;
			x[11] = -1.01036838713431;
			x[12] = -1.52417061939353;
			x[13] = -2.04923170985062;
			x[14] = -2.59113378979454;
			x[15] = -3.1578488183476;
			x[16] = -3.76218735196401;
			x[17] = -4.42853280660377;
			x[18] = -5.22027169053748;
			w[0] = 1.32629709449852e-12;
			w[1] = 2.16305100986354e-09;
			w[2] = 4.48824314722315e-07;
			w[3] = 2.72091977631616e-05;
			w[4] = 0.000670877521407184;
			w[5] = 0.00798886677772307;
			w[6] = 0.0508103869090527;
			w[7] = 0.183632701306996;
			w[8] = 0.391608988613028;
			w[9] = 0.502974888276183;
			w[10] = 0.391608988613033;
			w[11] = 0.183632701306998;
			w[12] = 0.0508103869090517;
			w[13] = 0.00798886677772294;
			w[14] = 0.000670877521407195;
			w[15] = 2.72091977631622e-05;
			w[16] = 4.48824314722318e-07;
			w[17] = 2.1630510098636e-09;
			w[18] = 1.32629709449851e-12;
		break;
		default:
			x[0] = 5.55035187326468;
			x[1] = 4.77399234341122;
			x[2] = 4.12199554749184;
			x[3] = 3.53197287713768;
			x[4] = 2.9799912077046;
			x[5] = 2.45355212451284;
			x[6] = 1.94496294918625;
			x[7] = 1.44893425065073;
			x[8] = 0.96149963441837;
			x[9] = 0.479450707079108;
			x[10] = 0;
			x[11] = -0.479450707079105;
			x[12] = -0.961499634418367;
			x[13] = -1.44893425065073;
			x[14] = -1.94496294918625;
			x[15] = -2.45355212451284;
			x[16] = -2.9799912077046;
			x[17] = -3.53197287713767;
			x[18] = -4.12199554749184;
			x[19] = -4.77399234341122;
			x[20] = -5.55035187326468;
			w[0] = 3.72036507013603e-14;
			w[1] = 8.81861124205004e-11;
			w[2] = 2.5712301800593e-08;
			w[3] = 2.17188489805666e-06;
			w[4] = 7.47839886731003e-05;
			w[5] = 0.00125498204172641;
			w[6] = 0.0114140658374345;
			w[7] = 0.0601796466589129;
			w[8] = 0.192120324066999;
			w[9] = 0.381669073613499;
			w[10] = 0.479023703120173;
			w[11] = 0.381669073613504;
			w[12] = 0.192120324067;
			w[13] = 0.0601796466589117;
			w[14] = 0.0114140658374341;
			w[15] = 0.0012549820417264;
			w[16] = 7.47839886730978e-05;
			w[17] = 2.17188489805664e-06;
			w[18] = 2.57123018005922e-08;
			w[19] = 8.81861124204988e-11;
			w[20] = 3.72036507013564e-14;
		break;
	}

  }
};

#endif /* GAUSSHERMITE_H */
