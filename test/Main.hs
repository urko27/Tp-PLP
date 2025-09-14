module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 8 "hola" ~?= "    hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 5 "" ~?= "     ",
      alinearDerecha 2 "quedaEntero" ~?= "quedaEntero"
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem (-1) (+ 10) [1, 2, 3] ~?= [1, 2, 3],
      actualizarElem 3 (+ 10) [1, 2, 3] ~?= [1, 2, 3]
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ]
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
   in test
        [ casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 8(agregar 5(agregar 2 (agregar 1 (agregar (-1) h0)))))
            ~?= [ Casillero infinitoNegativo 0 1 20, -- 20% de los valores en cada casillero
                  Casillero 0 2 1 20,
                  Casillero 2 4 1 20,
                  Casillero 4 6 1 20,
                  Casillero 6 infinitoPositivo 1 20
                ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      casilleros (histograma 1 (3.0, 5.0) []) ~?= [
        Casillero infinitoNegativo 3.0 0 0,
        Casillero 3.0 5.0 0 0,
        Casillero 5.0 infinitoPositivo 0 0
      ],
      casilleros (histograma 3 (2.0, 5.0) [2.2, 8.0, 2.3, 5.0, -1.0]) ~?= [
        Casillero infinitoNegativo 2.0 1 20.0,
        Casillero 2.0 3.0 2 40.0,
        Casillero 3.0 4.0 0 0,
        Casillero 4.0 5.0 0 0,
        Casillero 5.0 infinitoPositivo 2 40.0
      ],
      casilleros (histograma 2 (-5.0, 8.0) [1.0, 3.0, 3.0, 3.0, 3.0]) ~?= [
        Casillero infinitoNegativo (-5.0) 0 0,
        Casillero (-5.0) 1.5 1 20.0,
        Casillero 1.5 8.0 4 80.0,
        Casillero 8.0 infinitoPositivo 0 0
      ]
    ]

testsCasilleros :: Test
testsCasilleros =
  let equiprobable = agregar 1000.0 (agregar 4.7 (agregar 2.0 (agregar 1.5 (agregar (-5.0) (vacio 3 (0, 6))))))
  in test
    [ casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros equiprobable
        ~?= [ Casillero infinitoNegativo 0.0 1 20.0,
              Casillero 0.0 2.0 1 20.0,
              Casillero 2.0 4.0 1 20.0,
              Casillero 4.0 6.0 1 20.0,
              Casillero 6.0 infinitoPositivo 1 20.0
            ]
    ]

testsRecr :: Test
testsRecr =
  test
    [
      recrExpr                      -- Simplificamos la Expresión cuando hay operaciones triviales
        (\c -> Const c)             -- (sumar 0, multiplicar y dividir por 0, rangos que son 1 solo numero, etc)
        (\a b -> if a == b then (Const a) else (Rango a b))
        (\r1 r2 p q -> case (r1, r2) of
                         ((Const 0.0), _) -> r2
                         (_, (Const 0.0)) -> r1
                         _                -> Suma r1 r2)
        (\r1 r2 p q -> case (r1, r2) of
                         (_, (Const 0.0)) -> r1
                         _                -> Resta r1 r2)
        (\r1 r2 p q -> case (r1, r2) of
                         ((Const 1.0), _) -> r2
                         (_, (Const 1.0)) -> r1
                         _                -> Mult r1 r2)
        (\r1 r2 p q -> case (r1, r2) of
                         (_, (Const 1.0)) -> r1
                         _                -> Div r1 r2)
        (Div (Suma (Const 0.0) (Rango 1.0 1.0)) (Const 1.0)) ~?= (Const 1.0)
    ]

testsFold :: Test
testsFold =
  test
    [
      fst (foldExpr                   -- Sumamos 1 a los valores numéricos de la expresión
        (\c g -> (Const (c+1), g))
        (\a b g -> (Rango (a+1) (b+1), g))
        (\(resP, genP) (resQ, genQ) -> (Suma resP resQ, genQ))
        (\(resP, genP) (resQ, genQ) -> (Resta resP resQ, genQ))
        (\(resP, genP) (resQ, genQ) -> (Mult resP resQ, genQ))
        (\(resP, genP) (resQ, genQ) -> (Div resP resQ, genQ))
        (Suma (Const 2.0) (Rango 1 5)) genFijo) ~?= Suma (Const 3.0) (Rango 2 6),
        fst (foldExpr                 -- "Invertimos" las operaciones, suma <-> resta y mult <-> div
        (\c g -> (Const c, g))
        (\a b g -> (Rango a b, g))
        (\(resP, genP) (resQ, genQ) -> (Resta resP resQ, genQ))
        (\(resP, genP) (resQ, genQ) -> (Suma resP resQ, genQ))
        (\(resP, genP) (resQ, genQ) -> (Div resP resQ, genQ))
        (\(resP, genP) (resQ, genQ) -> (Mult resP resQ, genQ))
        (Div (Resta (Suma (Const 2.0) (Rango 1 5)) (Mult (Const 1.0) (Rango 4 8))) (Const 2.0)) genFijo) ~?=
          (Mult (Suma (Resta (Const 2.0) (Rango 1 5)) (Div (Const 1.0) (Rango 4 8))) (Const 2.0))
    ]

testsEval :: Test
testsEval =
  test
    [ fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      fst (eval (Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0))) (genNormalConSemilla 0)) ~?= 7.0,
      -- muestra de numeros: [2.7980492,3.1250308,5.464013,3.526857]
      fst (eval (
        Suma (Suma (Rango 1 5) (Rango 1 5)) (Suma (Rango 1 5) (Rango 1 5))
      ) (genNormalConSemilla 0)) ~?= 14.91395
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
    -- muestra (dameUno (1, 5)) 4 (fromList [1, 2, 3, 4])
    -- valores: [4.020408,5.0408163,6.0612245,7.0816326]
    -- rango95 (fst (muestra (dameUno (1, 5)) 4 (fromList [1, 2, 3])))
    -- rango: (3.3149521,7.7870884)
    [casilleros (fst (
      armarHistograma 3 4 (dameUno (1, 5)) (fromList ([1, 2, 3, 4]))
    )) ~?= [
        Casillero infinitoNegativo 3.3149521 0 0,
        Casillero 3.3149521 4.805664 1 25.0,
        Casillero 4.805664 6.296376 2 50.0,
        Casillero 6.296376 7.7870884 1 25.0,
        Casillero 7.7870884 infinitoPositivo 0 0.0
      ]
    ]

testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      -- por lo tanto va a estar en el casillero que incluya a 5.92308
      casilleros(fst(evalHistograma 3 1 (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0))) 
      ~?=
        [
          Casillero infinitoNegativo 4.92308 0 0.0,
          Casillero 4.92308 5.5897465 0 0.0,
          Casillero 5.5897465 6.2564135 1 100.0,
          Casillero 6.2564135 6.92308 0 0.0,
          Casillero 6.92308 infinitoPositivo 0 0.0
        ]
    ]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0",
      mostrar (Mult (Suma (Const 2) (Const 3)) (Const 4))
        ~?= "(2.0 + 3.0) * 4.0",
      mostrar (Mult (Const 2) (Suma (Const 3) (Const 4)))
        ~?= "2.0 * (3.0 + 4.0)",
      mostrar (Div (Div (Const 10) (Const 2)) (Const 5))
        ~?= "(10.0 / 2.0) / 5.0",
      mostrar (Div (Resta (Const 10) (Const 5)) (Const 2))
        ~?= "(10.0 - 5.0) / 2.0",
      mostrar (Div (Const 10) (Resta (Const 5) (Const 2)))
        ~?= "10.0 / (5.0 - 2.0)",
      mostrar (Resta (Const 10) (Mult (Const 5) (Const 2)))
        ~?= "10.0 - 5.0 * 2.0",
      mostrar (Suma (Mult (Resta (Const 5) (Const 2)) (Const 3)) (Div (Const 10) (Const 2)))
        ~?= "((5.0 - 2.0) * 3.0) + 10.0 / 2.0",
      mostrar (Div (Suma (Const 1) (Mult (Const 2) (Const 3))) (Suma (Const 4) (Const 5)))
        ~?= "(1.0 + 2.0 * 3.0) / (4.0 + 5.0)",
      mostrar (Resta (Resta (Rango 1 2) (Suma (Const 3) (Const 4))) (Mult (Const 5) (Suma (Const 6) (Const 7))))
        ~?= "(1.0~2.0 - (3.0 + 4.0)) - 5.0 * (6.0 + 7.0)"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
