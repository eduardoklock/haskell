module Formas (Forma (Esfera, Cilindro, Cone, TroncoCone, Oblato, Prolato),
Raio, RaioBase, RaioSeccao, Altura ,SemiEixoMaior, SemiEixoMenor, area, volume)
where

data Forma = Esfera Raio
	     | Cilindro Raio Altura
	     | Cone Raio Altura
   	     | TroncoCone RaioBase RaioSeccao Altura
	     | Oblato SemiEixoMaior SemiEixoMenor
	     | Prolato SemiEixoMaior SemiEixoMenor
	deriving Show

type Raio = Float
type RaioBase = Float
type RaioSeccao = Float
type Altura = Float
type SemiEixoMaior = Float
type SemiEixoMenor = Float


--Definindo area lateral para tornar as formulas mais legiveis
areaLat :: Forma -> Float
areaLat (Cilindro r h) = 2*pi*r*h
areaLat (Cone r h) = pi*r *sqrt(r^2+h^2)
areaLat (TroncoCone r1 r2 h) = pi*(r1+r2)*sqrt(h^2*(r1-r2)^2)

--Definindo as formulas de area
area :: Forma -> Float 
area (Esfera r) = 4*pi*r^2
area (Cilindro r areaLat) = areaLat+2*pi*r^2
area (Cone r h) = pi*r*sqrt(r^2+h^2)+r
area (TroncoCone r1 r2 h) = pi*r1^2+pi*r2^2+areaLat (TroncoCone r1 r2 h)
area (Oblato s1 s2) = 2*pi*s1^2+s2^2/(sqrt(s1^2+s2^2)/s1)+log(1+(sqrt(s1^2+s2^2)/s1)/1-(sqrt(s1^2+s2^2)/s1))
area (Prolato s1 s2) = 2*pi*s2^2+2*pi*(s1*s2/(sqrt(s1^2+s2^2)/s1))*asin(sqrt(s1^2+s2^2)/s1)   

--Definindo as formulas de volume
volume :: Forma -> Float
volume (Esfera r) = 4/3*pi*(r^3)
volume (Cilindro r h) = pi*(r^2)*h
volume (Cone r h) = 1/3*pi*(r^2)*h
volume (TroncoCone r1 r2 h)  = 1/3*pi*h*((r1^2)+(r2^2)+r1*r2)
volume (Oblato r1 r2) = 4/3*pi*(r1^2)*r2
volume (Prolato r1 r2) = 4/3*pi*r1*r2^2
