module Formas ( Forma (Esfera, Cilindo, Cone, Tronco, Oblato, Prolato),
                raio, raioBase, raioSeccao, altura, semiEixoMaior, semiEixoMenor) where

    data Forma = 
        Esfera raio |
        Cilindro raio altura |
        Cone raio altura |
        Tronco raioBase raioSeccao altura |
        Oblato semiEixoMaior semiEixoMenor |
        Prolato semiEixoMaior semiEixoMenor

        deriving Show
            type raio = Float 
            type altura = Float
            type raioBase = Float
            type raioSeccao = Float
            type semiEixoMaior = Float
            type semiEixoMenor = Float
    
-- formulas area
area :: Forma -> Float
area (Esfera raio) = (4*pi)*(raio^2)
area (Cilindro raio altura) = areaLateralCilindro raio altura + 2*pi*(raio^2)
area (Cone raio altura) = (pi*raio)*(sqrt(raio^2 + altura^2) + raio)
area (Tronco raioBase raioSeccao altura) = pi(raioBase^2 + raioSeccao^2) + areaLateralTronco raioBase raioSeccao altura
-- oblato
-- prolato

-- formulas volume
volume :: Forma -> Float
volume (Esfera raio) = (4*pi*(raio^3))/3
volume (Cilindro raio altura)  = pi*(raio^2)*altura
volume (Cone raio altura) = (1*pi*(raio^2)*altura)/3
volume (Tronco raioBase raioSeccao altura) = pi*altura*(raioBase^2 + raioSeccao^2 + raioBase*raioSeccao)
-- oblato
-- proplato

-- formulas area lateral

-- formulas cilindro

areaLateralCilindro raio altura = 2*pi*raio*altura

-- formula cone

areaLateralCone raio altura = (pi*raio)*(sqrt (raio^2 + altura^2))

-- formula tronco

areaLateralTronco raioBase raioSeccao altura = pi*(raioBase + raioSeccao)*(sqrt(altura^2 + raioBase^2 + raioSeccao^2))

--formula esferoide oblato
 
excentricidade semiEixoMaior semiEixoMenor = (sqrt(semiEixoMaior^2 + semiEixoMenor^2))/semiEixoMaior


