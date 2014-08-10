% ----------------------------------------------------------------------
% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
% Contributed by Anthony Borla
% Modified to run with YAP by Glendon Holst
% ----------------------------------------------------------------------

:- yap_flag(unknown,error).

:- initialization(main).

main :-
   unix( argv([H|_]) ), number_atom(N,H),

   make_bodies(Bodies0),

   energy(Bodies0, EnergyStart),

   loop_advance(N, 0.01, Bodies0, Bodies),

   energy(Bodies, EnergyAfter),

   format('~9f~N~9f~N', [EnergyStart, EnergyAfter]),

   statistics,
   statistics_jit.

% ------------------------------- %

energy(Bodies, Energy) :- energy_(Bodies, 0.0, Energy).

energy_([ _:B | Bs], Energy0, Energy) :-
    !, B = [_X, _Y, _Z, VX, VY, VZ, Mass],
    Energy1 is Energy0 + 0.5 * Mass * (VX * VX + VY * VY + VZ * VZ),
   energy_diff_(B, Bs, Energy1, Energy2),
   energy_(Bs, Energy2, Energy).

energy_([], Energy, Energy).

energy_diff_(Planet, [_:B | Bs], Energy0, Energy) :-
   Planet = [X, Y, Z, _VX, _VY, _VZ, Mass],
   B = [XT, YT, ZT, _VXT, _VYT, _VZT, MassT],
   DX is X - XT, DY is Y - YT, DZ is Z - ZT,
   DISTANCE is sqrt(DX * DX + DY * DY + DZ * DZ),
   Energy1 is Energy0 - (Mass * MassT) / DISTANCE,
   energy_diff_(Planet, Bs, Energy1, Energy).

energy_diff_(_, [], Energy, Energy).

% ------------------------------- %

loop_advance(N, Dt, Bodies0, Bodies) :-
   N > 0, !,
   advance(Dt, Bodies0, Bodies1),
   N1 is N - 1,
   loop_advance(N1, Dt, Bodies1, Bodies).

loop_advance(_, _, Bodies, Bodies).

advance(Dt, Bodies0, Bodies) :-
   Bodies0 = [B0 | B0s], !,
   advance_(Dt, B0, B1, B0s, B1s),
   advance(Dt, B1s, Bs),
   B1 = E:[X, Y, Z, VX, VY, VZ, Mass],
   X1 is X + Dt * VX,
   Y1 is Y + Dt * VY,
   Z1 is Z + Dt * VZ,
   B = E:[X1, Y1, Z1, VX, VY, VZ, Mass],
   Bodies = [ B | Bs].

advance(_, Bodies, Bodies).

advance_(Dt, Planet0, Planet, Bodies0, Bodies) :-
   Bodies0 = [B0 | B0s], !,
   Planet0 = E:[X, Y, Z, VX, VY, VZ, Mass],
   B0 = ET:[XT, YT, ZT, VXT, VYT, VZT, MassT],

   DX is X - XT, DY is Y - YT, DZ is Z - ZT,
   DISTANCE is sqrt(DX * DX + DY * DY + DZ * DZ),
   Mag is Dt / (DISTANCE * DISTANCE * DISTANCE),

   VX1 is VX - DX * MassT * Mag,
   VY1 is VY - DY * MassT * Mag,
   VZ1 is VZ - DZ * MassT * Mag,
   VXT1 is VXT + DX * Mass * Mag,
   VYT1 is VYT + DY * Mass * Mag,
   VZT1 is VZT + DZ * Mass * Mag,

   Planet3 = E:[X, Y, Z, VX1, VY1, VZ1, Mass],
   advance_(Dt, Planet3, Planet, B0s, Bs),

   B = ET:[XT, YT, ZT, VXT1, VYT1, VZT1, MassT],
   Bodies = [B | Bs].

advance_(_, P, P, Bs, Bs).

% ------------------------------- %

make_bodies(Bodies) :-
   SOLAR_MASS = 3.9478417604357432000e+01,
   Bodies0 =
   [
      jupiter:[4.84143144246472090e+00, -1.16032004402742839e+00,
         -1.03622044471123109e-01, 6.06326392995832020e-01,
         2.811986844916260200e+00, -2.5218361659887636e-02,
         3.7693674870389486e-02],
      saturn:[8.34336671824457987e+00, 4.12479856412430479e+00,
         -4.03523417114321381e-01, -1.010774346178792400e+00,
         1.825662371230411900e+00, 8.415761376584154e-03,
         1.1286326131968767e-02],
      uranus:[1.28943695621391310e+01, -1.51111514016986312e+01,
         -2.23307578892655734e-01, 1.082791006441535600e+00,
         8.68713018169607890e-01, -1.0832637401363636e-02,
         1.723724057059711e-03],
      neptune:[1.53796971148509165e+01, -2.59193146099879641e+01,
         1.79258772950371181e-01, 9.79090732243897980e-01,
         5.94698998647676060e-01, -3.4755955504078104e-02,
         2.033686869924631e-03]
   ],

   Sun0 = sun:[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SOLAR_MASS],
   offset_momentum(Sun0, Sun, Bodies0, SOLAR_MASS),
   Bodies = [Sun | Bodies0].

% ------------- %

offset_momentum(Sun0, Sun, Bodies, SOLAR_MASS) :-
   offset_momentum_(Bodies, [0.0, 0.0, 0.0], [PX, PY, PZ]),
   Sun0 = E:[X, Y, Z, VX, VY, VZ, Mass],
   VX1 is -(PX / SOLAR_MASS),
   VY1 is -(PY / SOLAR_MASS),
   VZ1 is -(PZ / SOLAR_MASS),
   Sun = E:[X, Y, Z, VX1, VY1, VZ1, Mass].

offset_momentum_([_:E|Bs], Pt0, Pt) :-
   E = [_X, _Y, _Z, VX, VY, VZ, Mass],
   Pt0 = [PX, PY, PZ],
   PX1 is PX + VX * Mass,
   PY1 is PY + VY * Mass,
   PZ1 is PZ + VZ * Mass,
   offset_momentum_(Bs, [PX1, PY1, PZ1], Pt).

offset_momentum_([], Pt, Pt).

% ------------------------------- %
