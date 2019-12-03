module SunP1Def where
import MathObj.LaurentPolynomial as LP
import EdgeSpaces

sunP1 :: EdgeSpace (LP.T Int)
sunP1 = EdgeSpace (fromCoeffs [1]) [ Down  (I 'A', E 1),
                                     Gluon (I 'A', I 'B'),
                                     Up    (I 'A', E 2),
                                     Up    (I 'B', E 3),
                                     Down  (I 'B', E 4) ]

sunP1LHS :: EdgeSpace (LP.T Int)
sunP1LHS = EdgeSpace (fromCoeffs [1]) [ Down (E 3, E 1),
                                        Down (E 2, E 4) ]
sunP1RHS :: EdgeSpace (LP.T Int)
sunP1RHS = EdgeSpace (fromShiftCoeffs (-1) [-1]) [ Up (E 1, E 2),
                                                   Down (E 3, E 4) ]

sunP1New :: EdgeSpaces (LP.T Int)
sunP1New = EdgeSpaces [sunP1LHS, sunP1RHS]
