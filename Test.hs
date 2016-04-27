--
-- Projecto CP 2015/16
--
-- O projecto consiste em desenvolver testes para o módulo Graph.hs
-- (para grafos orientados e não pesados).
-- Mais concretamente, o projecto consiste em 3 tarefas que são descritas abaixo.
-- O prazo para entrega é o dia 3 de Abril. Cada grupo deve enviar apenas
-- o módulo de testes (este módulo) por email para calculodeprogramas@gmail.com
-- O nome do ficheiro deve identificar os números dos 2 alunos do grupo (numero1_numero2.hs).
-- Certifiquem-se que o módulo de testes desenvolvido compila correctamente antes
-- de submeter. O módulo Graph.hs não deve ser alterado.
-- Os 2 alunos do grupo devem também indentificar-se nos comentários abaixo.
--
-- Aluno 1
-- Número: A75037
-- Nome: Catarina Cardoso
-- Curso: MIEI
--
-- Aluno 2
-- Número: Pedro Oliveira
-- Nome: A75521
-- Curso: MIEI
--


module Main where

import Graph
import Test.HUnit hiding (path)
import Test.QuickCheck
import Data.Set as Set

--
-- Teste vazio
--
g0 :: Graph Int
g0 = Graph.empty
    
--
-- Teste unitário
--
g1 :: Graph Int
g1 = Graph {nodes = fromList [1],
            edges = fromList [Edge 1 1]}

g2 :: Graph Int
g2 = Graph {nodes = fromList [1,2],
            edges = fromList [Edge 1 2]}

tg2 :: Graph Int
tg2 = Graph {nodes = fromList [1,2],
            edges = fromList [Edge 2 1]}

g3 :: Graph Int
g3 = Graph {nodes = fromList [1,2,3],
            edges = fromList [Edge 1 2, Edge 2 3]}

--
-- Teste inválido
--
g4 :: Graph Int
g4 = Graph {nodes = fromList [1],
            edges = fromList [Edge 1 2]}

tg4 :: Graph Int
tg4 = Graph {nodes = fromList [1],
            edges = fromList [Edge 2 1]}

g5 :: Graph Int
g5 = Graph {nodes = fromList [1,2,3],
            edges = fromList [Edge 1 2, Edge 2 3, Edge 3 1]}

g6 :: Graph Int
g6 = Graph {nodes = fromList [1,2,3],
            edges = fromList [Edge 1 2]}

g7 :: Graph Int
g7 = Graph {nodes = fromList [1,2,3],
            edges = fromList [Edge 1 2, Edge 1 3]}

tg7 :: Graph Int
tg7 = Graph {nodes = fromList [1,2,3],
             edges = fromList [Edge 2 1, Edge 3 1]}

g8 :: Graph Int
g8 = Graph {nodes = fromList [1,2],
            edges = fromList [Edge 1 1, Edge 1 2]}

g9 :: Graph Int
g9 = Graph {nodes = fromList [1,2,3],
            edges = fromList [Edge 1 2, Edge 1 3, Edge 2 1, Edge 3 1]}

g10 :: Graph Int
g10 = Graph {nodes = fromList [1,2,3,4,5],
             edges = fromList [Edge 1 2, Edge 1 5, Edge 2 3, Edge 5 4, Edge 4 3]}

g11 :: Graph Int
g11 = Graph {nodes = fromList [1,2,3,4,5],
             edges = fromList [Edge 2 3, Edge 2 1, Edge 1 4, Edge 1 5]}

g12 :: Graph Int
g12 = Graph {nodes = fromList [1,2,3,4],
             edges = fromList [Edge 1 2, Edge 3 4]}



test_swap :: Test
test_swap = TestList [swap (Edge 1 1) ~?= (Edge 1 1),
                      swap (Edge 1 2) ~?= (Edge 2 1),
                      swap (swap (Edge 2 3)) ~?= (Edge 2 3)]

test_isEmpty :: Test
test_isEmpty = TestList [isEmpty g0 ~?= True,
                         isEmpty g1 ~?= False]

test_isValid :: Test
test_isValid = TestList [isValid g0 ~?= True,
                         isValid g1 ~?= True,
                         isValid g6 ~?= True,
                         --isValid g3 ~?= True,
                         isValid g4 ~?= False]

test_isDAG :: Test
test_isDAG = TestList [isDAG g0 ~?= True,
                       isDAG g1 ~?= False,
                       isDAG g2 ~?= True,
                       isDAG g3 ~?= True,
                       isDAG g5 ~?= False]

test_isForest :: Test
test_isForest = TestList [isForest g0 ~?= True,
                          isForest g2 ~?= True,
                          isForest g3 ~?= True,
                          isForest g6 ~?= True,
                          isForest g7 ~?= False,
                          isForest tg7 ~?= True]

test_isSubgraphOf :: Test
test_isSubgraphOf = TestList [isSubgraphOf g0 g0 ~?= True,
                              isSubgraphOf g1 g1 ~?= True,
                              isSubgraphOf g2 g3 ~?= True,
                              isSubgraphOf g2 g6 ~?= True,
                              isSubgraphOf g1 g7 ~?= False]

test_adj :: Test
test_adj = TestList [adj g0 1 ~?= fromList [],
                     adj g1 1 ~?= fromList [Edge 1 1],
                     adj g2 1 ~?= fromList [Edge 1 2],
                     adj g2 2 ~?= fromList [],
                     adj g7 1 ~?= fromList [Edge 1 2, Edge 1 3]]

test_transpose :: Test
test_transpose = TestList [transpose g0 ~?= g0,
                           transpose g1 ~?= g1,
                           transpose g2 ~?= tg2,
                           transpose g4 ~?= tg4,
                           transpose g7 ~?= tg7,
                           transpose (transpose g5) ~?= g5]

test_union :: Test
test_union = TestList [Graph.union g0 g0 ~?= g0,
                       Graph.union g1 g1 ~?= g1,
                       Graph.union g1 g2 ~?= g8,
                       Graph.union g7 tg7 ~?= g9]

test_bft :: Test
test_bft = TestList [bft g0 (fromList []) ~?= g0,
                     bft g1 (fromList []) ~?= g0,
                     bft g6 (fromList [3]) ~?= Graph {nodes = fromList [3],
                                                      edges = fromList []},
                     bft g10 (fromList [5]) ~?= Graph {nodes = fromList [3,4,5],
                                                      edges = fromList [Edge 3 4, Edge 4 5]},
                     bft g12 (fromList [1,3]) ~?= Graph {nodes = fromList [1,2,3,4],
                                                         edges = fromList [Edge 2 1, Edge 4 3]}]

test_reachable :: Test
test_reachable = TestList [reachable g1 1 ~?= fromList [1],
                           reachable g5 1 ~?= fromList [1,2,3],
                           reachable g6 1 ~?= fromList [1,2],
                           reachable g6 3 ~?= fromList [3],
                           reachable g7 1 ~?= fromList [1,2,3],
                           reachable g8 1 ~?= fromList [1,2]]

test_isPathOf :: Test
test_isPathOf = TestList [isPathOf [] g0 ~?= True,
                          isPathOf [Edge 1 2] g1 ~?= False,
                          isPathOf [Edge 1 2] g4 ~?= True,
                          isPathOf [Edge 1 2, Edge 2 3, Edge 3 1] g5 ~?= True]

test_path :: Test
test_path = TestList [path g1 1 1 ~?= Just [],
                      path g5 1 3 ~?= Just [Edge 1 2, Edge 2 3],
                      path g6 1 3 ~?= Nothing]

test_topo :: Test
test_topo = TestList [topo g0 ~?= [],
                      topo g7 ~?= [fromList [1], fromList [2,3]],
                      topo tg7 ~?= [fromList [2,3], fromList [1]],
                      topo g10 ~?= [fromList [1], fromList [2,5], fromList [4], fromList [3]],
                      topo g11 ~?= [fromList [2], fromList [1,3], fromList [4,5]],
                      topo g12 ~?= [fromList [1,3], fromList [2,4]]]

--
-- Tarefa 1
--
-- Defina testes unitários para todas as funções do módulo Graph,
-- tentando obter o máximo de cobertura de expressões, condições, etc.
--
           
main = runTestTT $ TestList [test_swap,
                             test_isEmpty,
                             test_isValid,
                             test_isDAG,
                             test_isForest,
                             test_isSubgraphOf,
                             test_adj,
                             test_transpose,
                             test_union,
                             test_bft,
                             test_reachable,
                             test_isPathOf,
                             test_path,
                             test_topo]








--
-- Teste aleatório
--

--
-- Tarefa 2
--
-- A instância de Arbitrary para grafos definida abaixo gera grafos
-- com muito poucas arestas, como se pode constatar testando a
-- propriedade prop_valid.
-- Defina uma instância de Arbitrary menos enviesada.
-- Este problema ainda é mais grave nos geradores dag e forest que
-- têm como objectivo gerar, respectivamente, grafos que satisfazem
-- os predicados isDag e isForest. Estes geradores serão necessários
-- para testar propriedades sobre estas classes de grafos.
-- Melhore a implementação destes geradores por forma a serem menos enviesados.
--

-- Instância de Arbitrary para arestas
instance Arbitrary v => Arbitrary (Edge v) where
    arbitrary = do s <- arbitrary
                   t <- arbitrary
                   return $ Edge {source = s, target = t}

instance (Ord v, Arbitrary v) => Arbitrary (Graph v) where
    arbitrary = aux `suchThat` isValid
        where aux = do ns <- arbitrary
                       es <- arbitrary
                       return $ Graph {nodes = fromList ns, edges = fromList es}
 
prop_valid :: Graph Int -> Property
prop_valid g = collect (length (edges g)) $ isValid g

-- Gerador de DAGs
dag :: (Ord v, Arbitrary v) => Gen (DAG v)
dag = arbitrary `suchThat` isDAG

prop_dag :: Property
prop_dag = forAll (dag :: Gen (DAG Int)) $ \g -> collect (length (edges g)) $ isDAG g

-- Gerador de florestas
forest :: (Ord v, Arbitrary v) => Gen (Forest v)
forest = arbitrary `suchThat` isForest

prop_forest :: Property
prop_forest = forAll (forest :: Gen (Forest Int)) $ \g -> collect (length (edges g)) $ isForest g

--
-- Tarefa 3
--
-- Defina propriedades QuickCheck para testar todas as funções
-- do módulo Graph.
--

-- Exemplo de uma propriedade QuickCheck para testar a função adj          
prop_adj :: Graph Int -> Property
prop_adj g = forAll (elements $ elems $ nodes g) $ \v -> adj g v `isSubsetOf` edges g
