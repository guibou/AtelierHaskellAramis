===============================================================================================
Comment Haskell a changé ma vision de développeur sur de nombreux points et notamment le typage
===============================================================================================

:auteur: Guillaume Bouchard
:date: 15 mars 2016

.. note ::

   - Bonjour. Présentation
   - Dans cette session je vais presenter Haskell
   - En me basant sur des exemples de developement dirigé par les types
   - J'espere pouvoir montrer que le typage est un outil puissant de conception, et particulièrement dans un langage comme Haskell

Tête la première : ET logique
=============================

.. literalinclude :: And.hs
   :language: haskell
   :lines: 3-4

.. code-block:: ghci

   Prelude> binaryAnd True False
   False
   Prelude> binaryAnd True True
   True

.. note ::

   - Observer la syntaxe où je liste les cas
   - Ceux-ci sont executés de haut en bas jusqu'à trouver celui qui marche
   - `_` est un joker.

Tête la première : ET sur une liste
===========================================

.. literalinclude :: And.hs
   :language: haskell
   :lines: 3-7

.. code-block:: ghci

   Prelude> binaryAnds []
   True
   Prelude> binaryAnds [True, False, True]
   False
   Prelude> binaryAnds [True, True, True]
   True

.. note ::

   - Ceci est une évaluation dans le shell interactif `GHCI`.
   - Fonctionalité importante qui permet de tester / évaluer interactivement son code

Tête la première : Types statiques
========================================

Types verifiés statiquements.

.. code-block :: ghci

   Prelude> binaryAnd "Hello" True

   <interactive>:23:11:
       Couldn't match expected type ‘Bool’ with actual type ‘[Char]’
       In the first argument of ‘binaryAnd’, namely ‘"Hello"’
       In the expression: binaryAnd "Hello" True
       In an equation for ‘it’: it = binaryAnd "Hello" True

   Prelude> binaryAnd 0 True

   <interactive>:24:11:
       No instance for (Num Bool) arising from the literal ‘0’
       In the first argument of ‘binaryAnd’, namely ‘0’
       In the expression: binaryAnd 0 True
       In an equation for ‘it’: it = binaryAnd 0 True

.. note ::

   - Bien que je vous montre un cas dans le shell interactif, il s'agit bien de verification à la compilation.
   - Haskell peut quand même être executé en tant que script.
   - Le typage est statique et inferé à la compilation
   - Le typage est strict, 0 est different de False

Tête la première : Types inferés
========================================

Les types sont inferés par le compilateur.

.. literalinclude :: And.hs
   :language: haskell
   :lines: 3-7

.. code-block:: ghci

    Prelude> :type binaryAnd
    binaryAnd :: Bool -> Bool -> Bool

    Prelude> :type binaryAnds
    binaryAnds :: [Bool] -> Bool

.. 
    Introduction rapide
    ===================

    .. literalinclude :: fact.hs
    :language: haskell
    :lines: 1-3

    .. notes :
	- Définition d'une fonction `factorielle`. Notez la syntaxe avec énumeration des cas.

    Introduction rapide
    ===================

    .. literalinclude :: fact.hs
    :language: haskell
    :lines: 1-7

    .. notes :
	- Définition d'une fonction `main` qui appelle notre `factorielle`.
	- La définition d'alias est faite avec `let`.
	- Notez que ce code est statiquement typé bien que on ne remarque aucune notion de type.

    Introduction rapide
    ===================

    .. literalinclude :: factBroken.hs
    :language: haskell

    .. notes :
    - Erreur de type, "Hello" n'est pas un nombre.

    - Dans la suite de cette présentation j'insisterais sur le typage en tant qu'outil de developement

Plan de la présentation
=======================

- Présentation rapide d'Haskell
- Haskell, syntaxe et types, avec exemples
- Modélisation par les types, débat
- Live coding d'un exemple complexe

(À suivre : 4 slides de texte, le reste n'aura que du code ;)

Haskell : Points clés
=====================

- Syntaxe expressive, modulaire
- Rapide. Interprété ou compilé.
- Outils puissants (repl, package manager, test / couverture / performance...)
- Paresseux
- Système de type puissant et strict

  - Forte séparation entre calculs purs et effets de bords
  - Pas de cast implicite, (`0 /= [] /= False /= ""`)


.. note ::

  - Personelement je le trouve très expressif et agréable à programmer avec une capacitié à réutiliser enormement de code.
  - Excellente performances pour un langage managé. Sur de nombreux problèmes j'atteint les performances du C++, mais en moyenne je suis 2 fois plus lent.
  - Outils, le repl que vous connaissez, mais aussi un package manager (stack) entre autre.
  - Haskell est paresseux dans son évaluation. C'est une caractéristique rare qui merite d'être notée et qui change un peu la façon de raisonner.
  - Enfin, et ce sera le topic de la présentation, son système de type est très puissant

.. 
    Types en Haskell
    ================

    - Forte séparation entre calculs purs et effets de bords
    - Pas de cast implicite, (`0 /= [] /= False /= ""`)
    - Inférence de type puissante
    - Typage très expressif
    - Polymorphisme facile
    - Programation génerique
    - Découplage des types et du comportement

.. 
    Débat
    ========

    - Typage fort versus faible ?

    - Typage dynamique versus statique ?

    - Python versus Java / C++ ?

    .. note ::

    J'étais plutôt dans le camps des utilisateurs de python. Les types ne nous embetent pas, on ne déclare rien, et on espere avoir suffisament de tests unitaires pour nous proteger.

    Les types en C++ ou en Java je me suis toujours battu contre pour arriver à exprimer ce que je veux sans que le compilateur ne hurle.

    Depuis que je fais du Haskell, j'aime les types et j'y ai trouvé plus de sécurité qu'avec les types dynamiques de python, plus qu'avec ceux de Java ou C++ qui sont, à mon gout, terriblement faible et inexpressif, et j'y ai trouvé un gain de productivité et un plaisir à programmer.

    -}

Avantages du typage statique
============================

- Plus d'erreur vérifiées à la compilation
- Moins d'erreur à l'exécution
- Refactoring plus facile
- Outils (auto-completion, analyse statique)
- Performances ?
- Expressivité

Inconvénients du typage statique
================================

- Moins de souplesse ? (On peu quand même faire du typage dynamique en Haskell).

Votre avis ?

Syntaxe : Types primaires
=========================

.. code-block :: haskell

   foo :: Int -- ou `Integer` pour précision infinie
   foo = 5

   bar :: Float -- ou `Double`
   bar = 3.2

   ham :: Char
   ham = 'g'

   egg :: [Int]
   egg = [1, 2, 3, 4]

   spam :: String -- ou `[Char]`
   spam = "Bonjour"

- Annotations non obligatoires

.. note ::

   Les annotions de type ne sont pas obligatoires car Haskell infere le type tout seul.

   Cas particulié de `5` qui peut autant être un `Int` qu'un `Float` en fonction du contexte. Mais il n'y a pas de cast implicite.

Fonctions
===============

Définition :

.. code-block :: haskell

   add2 :: Int -> Int
   add2 x = x + 2


Usage :

.. code-block :: ghci

   Prelude> add2 5
   7

   Prelude> :type lengthString
   lengthString :: String -> Int

   Prelude> lengthString "bonjour"
   7

Polymorphisme
=============

.. code-block :: ghci

    Prelude> :type replicate
    replicate :: Int -> a -> [a]

    Prelude> replicate 5 0
    [0,0,0,0,0]

    Prelude> replicate 5 'g'
    "ggggg"

    Prelude> replicate 5 "Bonjour"
    ["Bonjour","Bonjour","Bonjour","Bonjour","Bonjour"]

Exemple de fonctions
====================

.. code-block :: haskell

   abs :: Int -> Int
   abs x
      | x < 0 = -x
      | otherwise = x

   length :: [a] -> Int
   length [] = 0
   length (x:xs) = (length xs) + 1

Polymorphism contraint
======================

.. code-block :: haskell

   min :: Ord t => t -> t -> t
   min a b = if a < b
               then a
               else b

Il existe de nombreuses "instances" de `Ord` et vous pouvez créer les votres à partir de n'importe quel type, nouveau ou existant.

Fonctions avancées
==================

.. code-block :: haskell

   duplicate = replicate 2

.. code-block :: ghci

   Prelude> duplicate "Bonjour"
   ["Bonjour","Bonjour"]
   Prelude> duplicate 5
   [5,5]

   Prelude> map length ["Bonjour", "Monde"]
   [7,5]

   Prelude> map (*2) [1..10]
   [2,4,6,8,10,12,14,16,18,20]

Example : Fonction partielle
======================================

Une fonction partielle peut ne pas être définie sur tout son domaine.

Quel est le minimum d'une liste vide ?

.. code-block :: haskell

   minimum []

Différentes Approches :

- Exception
- Valeur sentinelle arbitraire
- Valeur de retour par référence
- Type spécifique ?

Fonction partielle - Valeur sentinelle
============================================

.. code-block :: c++

   float minimum(const std::vector<float> &v)
   {
       if(v.size() == 0)
            return -std::numeric_limits<float>::infinity();

       float ret = v[0];
       for(size_t i = 1; i < v.size(); ++i)
       {
           ret = std::min(v[i], ret);
       }
       return ret;
   }

.. code-block :: c++

   if(minimum(maList) < 10)
   {
       std::cout << "BUG !" << std::endl;
   }

Fonction partielle - Exception
==============================

Exception (Python) :

.. code-block :: pycon

      >>> min([1,2,3])
      1
      >>> min([])
      Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
      ValueError: min() arg is an empty sequence

Bug : cas non gérés, erreur à l'exécution

Fonction partielle - *Flag* de réussite
=======================================

.. code-block :: cpp

   bool minimum(const std::vector<float> &v, float &ret)
   {
       if(v.size() == 0)
            return false;

       ret = v[0];
       for(size_t i = 1; i < v.size(); ++i)
           ret = std::min(v[i], ret);

       return true;
   }

Bugs :

.. code-block :: cpp

   float res;
   minimum(maListe, res);
   std::cout << res << std::endl; // Non initialisé

Solution : Type spécifique
========================================

- Utilisation du type polymorphique `Maybe t`.

.. code-block :: haskell

   minimum :: [Float] -> Maybe Float
   minimum [] = Nothing
   minimum l = Just (minimum' l)
      where minimum' [x] = x
            minimum' (x:xs) = min x (minimum' xs)

.. code-block :: ghci

   Prelude> minimum [2,5,-3,0]
   Just (-3)
   Prelude> minimum []
   Nothing

   Prelude> 2 * (minimum [1, 2, 3])
   <interactive>:11:7:
       Couldn't match expected type ‘Int’ with actual type ‘Maybe Integer’
       In the second argument of ‘(*)’, namely ‘(minimum [1 .. 5])’
       In the expression: (2 * (minimum [1 .. 5])) :: Int

.. note ::

   Le typage nous empêche d'utiliser la valeur sans vérifier.

`Maybe t` : usage
==================

.. code-block :: haskell

   case minimum maListe of
       Nothing -> 0
       Just v -> 2 * v

Solutions avancées :

.. code-block :: ghci

   Prelude> fmap (*2) Nothing
   Nothing
   Prelude> fmap (*2) (Just 10)
   Just 20

.. code-block :: haskell

   fmap (*2) (minimum maListe)

`fmap` applique une fonction à l'intérieur d'un `Functor` et renvoie quelque chose de la même forme. Généralisation de `map`.

Types custom
============

Example avec un type `Shape` :

.. code-block :: haskell

   data Shape = Rectangle Float Float | Square Float

   -- Construction
   myShape = Rectangle 5 10

   -- Deconstruction / fonction
   -- optionel: area :: Shape -> Float
   area (Rectangle a b) = a * b
   area (Square c) = c * c

"Pattern matching" obligatoire vérifié.

Arbre binaire
======================

Arbre binaire simple.

.. code-block :: haskell

   data Tree = Node Float Tree Tree | Leaf

   lenTree :: Tree -> Int
   lenTree Leaf = 0
   lenTree (Node _ t t') = lenTree t + lenTree t' + 1

.. code-block :: ghci

   Prelude> lenTree (Node 5 Leaf (Node 2 Leaf Leaf))
   2

.. note ::

   - Arbre non polymorphique

Arbre binaire polymorphique
=====================================

.. code-block :: haskell

   data Tree t = Node t (Tree t) (Tree t) | Leaf

   lenTree :: Tree t -> Int
   lenTree Leaf = 0
   lenTree (Node _ s s') = lenTree s + lenTree s' + 1

.. code-block :: ghci

   Prelude> lenTree (Node 5 Leaf (Node 2 Leaf Leaf))
   2
   Prelude> lenTree (Node "Hello" Leaf (Node "World" Leaf Leaf))
   2

Arbre binaire - usages ?
==================================

- Longueur de l'arbre
- Serialisation
- Opérations sur tous les elements (min, max...)
- Opérations sur l'arbre (transformations)
- Affichage (pour débug ?)

Imaginez le code nécessaire en C++ / Python ?

Haskell propose un mécanisme générique d'ajout de comportement surchargeable.

Exemple : Affichage génerique
=============================

.. code-block :: haskell

   data Tree t = Node t (Tree t) (Tree t) | Leaf deriving (Show)

.. code-block :: ghci

   Prelude> show (Node 5 Leaf (Node 2 Leaf Leaf))
   "(Node 5 Leaf (Node 2 Leaf Leaf))"

Opérations génériques
=====================================

.. code-block :: haskell

   data Tree t = Node t (Tree t) (Tree t) | Leaf
          deriving (Show, Functor, Foldable)

.. code-block :: ghci

   Prelude> let t = (Node 5 Leaf (Node 2 Leaf Leaf))
   Prelude> show t
   "Node 5 Leaf (Node 2 Leaf Leaf)"

   Prelude> length t
   2

   Prelude> fmap (*2) t
   Node 10 Leaf (Node 4 Leaf Leaf)

   Prelude> foldl (+) 0 t
   7

Arbre binaire - Sérialisation
=======================================

.. code-block :: haskell

   data Tree t = Node t (Tree t) (Tree t) | Leaf
          deriving (Show, Serialize, Generic)

.. code-block :: ghci

   Prelude> show t
   "Node 5 Leaf (Node 2 Leaf Leaf)"

   Prelude> encode t
   "\NUL\NUL\NUL\NUL\NUL\ENQ\SOH\NUL\NUL\NUL\NUL\NUL\STX\SOH\SOH"

   Prelude> (decode (encode t)) :: Either String (Tree Integer)
   Right (Node 5 Leaf (Node 2 Leaf Leaf))

.. note ::

   import Data.Serialize
   import GHC.Generics
   DeriveFunctor
   DeriveFoldable
   DeriveAnyClass
   DeriveGeneric

   Le `Either` c'est un `Maybe` avec erreur.

Effets dans le système de types
===============================

Que peuvent faire les fonctions python, C++ et haskell suivantes ?

.. code-block :: python

   def blork(a, b):
        ...

.. code-block :: cpp

   float blork(float a, float b)
   {
      ...
   }

.. code-block :: haskell

   blork :: Float -> Float -> Float
   blork a b = ...


Types : séparation des effets
=============================

.. code-block :: python

   def blork(a, b):
       # - n'importe quoi sur a et b, qui sont de n'importe quel type
       # - Renvoie n'importe quoi
       # Des IOs (variables globales, réseau, fichiers, ...)
       ...

.. code-block :: cpp

   float blork(float a, float b)
   {
       // - n'importe quoi sur a et b, qui sont de type float
       // - Renvoie un float
       // Des IOs (variables globales, réseau, fichiers, ...)
   }

.. code-block :: haskell

   blork :: Float -> Float -> Float
   blork a b = ... -- N'importe quoi sur a et b, des Float
               ... -- Renvoie un Float
               ... -- Pas d'IO !

Types : séparation des effets
=============================

La présence d'effets de bords apparaît dans le type.

.. code-block :: haskell

   sayHello who = putStrLn ("Hello you " ++ who)

   concat a b = a ++ b

   computeAge = do
      putStrLn "Quelle est votre année de naissance ?"
      annee <- getLine
      return (2016 - read annee)

.. code-block :: ghci

   Prelude> :t sayHello
   sayHello :: String -> IO ()

   Prelude> :t concat
   concat :: [a] -> [a] -> [a]

   Prelude> :t computeAge
   computeAge :: IO Int

Pureté : raisonement simplifié
==============================

Les codes C++, python suivants sont-ils équivalents ?

.. code-block :: cpp

   // version 1
   float a = blork(5, 2);
   float b = blork(5, 2);
   return a + b;

   // version 2
   float a = blork(5, 2);
   return a + a;

.. code-block :: python

   # version 1
   print("hello")
   notUsedVariable = doSomething(otherVariable)
   print("world")

   # version 2
   print("hello")
   print("world")

Pureté : raisonement simplifié
==============================

.. code-block :: haskell

   -- version 1
   let a = blork 5 2
       b = blork 5 2
   in a + b

   -- version 2
   let a = blork 5 2
   in a + a

.. code-block :: haskell

   -- version 1
   main = do
      putStrLn "hello"
      let notUsedVariable = doSomething otherVariable
      putStrLn "world"

   -- version 2
   main = do
      putStrLn "hello"
      putStrLn "world"

Modéliser par les types : Quickcheck
====================================

Generation automatique d'échantillons.


.. code-block :: ghci

   Prelude> generate (arbitrary :: Gen [Float])
   [-5.209145,-141.35745,8.049846,-40.25172,35.769672,18.124104,-27.154808,71.316666,-26.435165]
   
   Prelude> generate (arbitrary :: Gen [(Char, Int)])
   [('&',0),('\153',26),('W',24),('3',1),('\ETX',7),('\CAN',-2),('v',22),('K',-14),('!',-26),('5',15),('\ENQ',26),('\SUB',-5),('%',-17),('\SOH',16),('8',6),('U',14),('\235',0),('\183',-18),('\196',7),('\a',-3),('K',17),('=',13),('o',2),('o',-26),('B',-25),('\166',-25),('\154',28),('Y',18),('\\',0),('\178',23)]
   

.. code-block :: haskell

   import Test.QuickCheck
   
   data Shape = Rectangle Float Float | Square Float deriving (Show)

   instance Arbitrary Shape where
       -- (4 lignes)

.. code-block :: ghci

   Prelude> generate (arbitrary :: Gen [Shape])
   [Square 10.137317,Square 100.347984,Rectangle 9.437294 32.822086,Square 16.126654,Rectangle 21.215836 139.80946]

QuickCheck : Tests automatiques
===============================

.. code-block :: ghci
    
    Prelude> import Test.QuickCheck
    Prelude> import Data.List
    
    Prelude> quickCheck (\x -> x < x + 1)
    +++ OK, passed 100 tests.

    Prelude> quickCheck (\l -> sort l == sort (sort l))
    +++ OK, passed 100 tests.

    Prelude> quickCheck (\(NonEmpty l) -> minimum l == head (sort l))
    +++ OK, passed 100 tests.

    Prelude> let brokenFact n = if n == 12 then 1 else product [1..n]
    Prelude> quickCheck (\x -> brokenFact x <= brokenFact (x + 1))
    *** Failed! Falsifiable (after 34 tests):
    11
    Prelude> quickCheck (\x -> brokenFact x <= brokenFact (x + 1))
    +++ OK, passed 100 tests.


Modéliser par les types : Optparse-Generic
==========================================

- Création d'une interface ligne de commande similaire à git proposant plusieurs options avec différents arguments.

.. literalinclude :: OptParse.hs
   :language: haskell
   :lines: 8-100

Optparse-Generic
================

.. code-block :: shell

   $ ./OptParse move --from thisPath --to otherPath
   Move {from = "thisPath", to = "otherPath"}
   $ ./OptParse commit          
   Commit {message = Nothing}
   $ ./OptParse commit --message "Test"
   Commit {message = Just "Test"}
   $ ./OptParse checkout --branch aBranch
   Checkout {branch = "aBranch", flag = False}
   $ ./OptParse checkout --branch aBranch --flag
   Checkout {branch = "aBranch", flag = True}

   $ ./OptParse checkout --help
   Usage: OptParse checkout --branch STRING [--flag]
   
   Available options:
      -h,--help                Show this help text

Conception typage fort
======================

Problème, définir les structures pour un lancer de rayon :

.. graphviz ::

   digraph G {
        node [
                shape = "record"
        ]

        Rayon [
                label = "{Rayon|+ origine\l+ direction}"
        ]

        Sphere [
                label = "{Sphere|+ centre\l+ diamètre : Float\l+ couleur}"
        ]

        Intersection [
                label = "{Intersection|+ point\l+ normale\l+ objet : Sphere}"
        ]
   }

.. code-block :: haskell

   data Rayon = Rayon {origine :: ?, direction :: ?}
   data Sphere = Sphere {centre :: ?, diamètre :: Float, couleur :: ?}
   data Intersection = Intersection {point :: ?, normale :: ?,
                                     objet :: Sphere}

   intersect :: Rayon -> Sphere -> Maybe Intersection

Conception typage fort
======================

Problème, définir les structures pour un lancer de rayon :

.. graphviz ::

   digraph G {
        node [
                shape = "record"
        ]

        Rayon [
                label = "{Rayon|+ origine : Vector\l+ direction : Vector}"
        ]

        Sphere [
                label = "{Sphere|+ centre : Vector \l+ diamètre : Float\l+ couleur : Vector}"
        ]

        Intersection [
                label = "{Intersection|+ point : Vector\l+ normale : Vector\l+ objet : Sphere}"
        ]

        Vector [
	        label = "{Vector|+ x : Float\l+ y : Float\l+ z : Float}"
        ]
   }

.. code-block :: haskell

   data Rayon = Rayon {origine :: Vector, direction :: Vector}
   data Sphere = Sphere {centre :: Vector, diamètre :: Float,
                         couleur : Vector}
   data Intersection = Intersection {point :: Vector, normale :: Vector,
                                     objet :: Sphere}

   data Vector = Vector {x :: Float, y :: Float, z :: Float}

   intersect :: Rayon -> Sphere -> Maybe Intersection

Fonction de `Vector`
====================

.. code-block :: haskell

   -- Operations terme à terme
   (^+^) :: Vector -> Vector -> Vector
   (^-^) :: Vector -> Vector -> Vector
   (^*^) :: Vector -> Vector -> Vector
   (^/^) :: Vector -> Vector -> Vector
   negate :: Vector -> Vector

   -- Operations scalaires
   (*^) :: Float -> Vector -> Vector
   (^*) :: Vector -> Float -> Vector
   (/^) :: Float -> Vector -> Vector
   (^/) :: Vector -> Float -> Vector

   -- Opération vectorielles
   dot :: Vector -> Vector -> Float
   cross :: Vector -> Vector -> Vector
   norm :: Vector -> Float
   normalize :: Vector -> Vector
   -- Utilitaires
   isBlack :: Vector -> Bool

Critique de la modélisation
===========================

.. graphviz ::

   digraph G {
        node [
                shape = "record"
        ]

        Rayon [
                label = "{Rayon|+ origine : Vector\l+ direction : Vector}"
        ]

        Sphere [
                label = "{Sphere|+ centre : Vector \l+ diamètre : Float\l+ couleur : Vector}"
        ]

        Intersection [
                label = "{Intersection|+ point : Vector\l+ normale : Vector\l+ objet : Sphere}"
        ]

        Vector [
	        label = "{Vector|+ x : Float\l+ y : Float\l+ z : Float}"
        ]
   }

- 4 types bien clairs (`Rayon`, `Sphere`, `Intersection`, `Vector`)
- Réutilisation de code maximal (`Vector`)
- Heureux ?

Critique de la modélisation
===========================

.. graphviz ::

   digraph G {
        node [
                shape = "record"
        ]

        Rayon [
                label = "{Rayon|+ origine : Vector\l+ direction : Vector}"
        ]

        Sphere [
                label = "{Sphere|+ centre : Vector \l+ diamètre : Float\l+ couleur : Vector}"
        ]

        Intersection [
                label = "{Intersection|+ point : Vector\l+ normale : Vector\l+ objet : Sphere}"
        ]

        Vector [
	        label = "{Vector|+ x : Float\l+ y : Float\l+ z : Float}"
        ]
   }

- Bug possibles :
   - confondre direction / origine du rayon / position de la sphere / couleur / ....
   - Ajouter une direction à une couleur ?
   - ...

Nouvelle modélisation
=====================

.. graphviz ::

   digraph G {
        node [
                shape = "record"
        ]

        Rayon [
                label = "{Rayon|+ origine : Point\l+ direction : Direction}"
        ]

        Sphere [
                label = "{Sphere|+ centre : Point \l+ diamètre : Float\l+ couleur : Couleur}"
        ]

        Intersection [
                label = "{Intersection|+ point : Point\l+ normale : Normal\l+ objet : Sphere}"
        ]

        Vector [
	        label = "{Vector|+ x : Float\l+ y : Float\l+ z : Float}"
        ]

        Point {
	        label = "{Point|+ : Vector}"
	}

        Normal {
	        label = "{Normal|+ : Vector}"
	}

        Direction {
	        label = "{Direction|+ : Vector}"
	}

        Couleur {
	        label = "{Couleur|+ : Vector}"
	}

        Point -> Vector
        Normal -> Vector
        Direction -> Vector
        Couleur -> Vector
   }

.. code-block :: haskell

   data Rayon = Rayon {origine :: Point, direction :: Direction}
   data Sphere = Sphere {centre :: Point, diamètre :: Float,
                         couleur : Couleur}
   data Intersection = Intersection {point :: Point, normale :: Normal,
                                     objet :: Sphere}
   data Vector = Vector {x :: Float, y :: Float, z :: Float}
   intersect :: Rayon -> Sphere -> Maybe Intersection

   data Point = Point Vector
   data Normal = Normal Vector
   data Direction = Direction Vector
   data Couleur = Couleur Vector

Nouvelle fonctions
====================

.. code-block :: haskell

   -- Point
   distance :: Point -> Point -> Float -- norm (a ^-^ b)
   directionFromTo :: Point -> Point -> Direction -- (b ^-^ a)
   translateScaled :: Point -> Direction -> Float -> Point -- (a ^+^ (b ^* c))

   -- Direction
   negate :: Direction -> Direction
   dot :: Direction -> Direction -> Float
   norm :: Direction -> Float
   normalize :: Direction -> Normal

   -- Normal
   negate :: Normal -> Normal
   dot :: Normal -> Normal -> Float
   cross :: Normal -> Normal -> Direction

   -- Couleur
   -- ...
   isBlack :: Couleur -> Bool

Bilan
-------

- 14 fonctions -> 18
- Point (14 -> 3)
- Direction (14 -> 4)
- Normal (14 -> 3)
- Couleur (14 -> 8)

- Implémentation aisée. Exemple :

.. code-block :: haskell

  distance (Point v0) (Point v1) = norm (v0 ^-^ v1)

  directionFromTo (Point from) (Point to) = Direction (to ^-^ from)

.. note ::

   - Grosse réduction du nombre de fonction par type
   - Moins d'erreur possible
   - Noms plus clairs dans de nombreux cas (translate versus +, directionFromTo versus -...)

Évaluation paresseuse
=====================

Live dans le shell..

.. note ::

   .. 
    :l Long.hs

    and [lc False, lc True, lc False, lc False] -- 1s

    0 `elem` [lc 0, lc 2, lc 5, lc 8] -- 1s

Typage de quantités
===================

<https://hackage.haskell.org/package/units>

<https://hackage.haskell.org/package/dimensional>

- Grandeurs physiques et unités typés pour ne pas mélanger les choux et les carottes


Point négatifs d'Haskell
========================

- Documentation des librairies étonnante, mais typée
- Abus d'opérateurs.
- L'évaluation paresseuse

  - Performances
  - Utilisation mémoire
  - Ordre d'évaluation inconnu

- Quelques surprises, comme les les IO lazy.
- Certains outils (le debugeur...)
- Portabilité limitée (intel 32/64 bits, arm..., javascript)
- Runtime

Points Non traités
====================

- Concurrence
- Analyse statique (Liquid Haskell)
- Hoogle <http://haskell.org/hoogle>
- ... Vos questions ? --> Live Coding

============
Live Coding
============

Live coding du problème 7 d'aventOfCode <http://adventofcode.com/day/7>

.. 
    Résumé du problème, évaluer :

    ::
	123 -> x
	456 -> y
	x AND y -> d
	x OR y -> e
	x LSHIFT 2 -> f
	y RSHIFT 2 -> g
	NOT x -> h
	NOT y -> i

    - Wire are 16 bits unsigned values

    .. note :: results

    d: 72
    e: 507
    f: 492
    g: 114
    h: 65412
    i: 65079
    x: 123
    y: 456
