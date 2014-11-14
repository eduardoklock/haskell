-- formulas circulo

areaCirculo raio = (4*pi)*(raio^2)

volumeCirculo raio = (4*pi*(raio^3))/3

-- formulas cilindro

areaLateralCilindro raio altura = 2*pi*raio*altura

areaTotalCilindro raio altura = areaLateralCilindro raio altura + 2*pi*(raio^2)

volumeCilindro raio altura  = pi*(raio^2)*altura

-- formula cone

areaLateralCone raio altura = (pi*raio)*(sqrt (raio^2 + altura^2))

areaTotalCone raio altura = (pi*raio)*(sqrt(raio^2 + altura^2) + raio)

volumeCone raio altura = (1*pi*(raio^2)*altura)/3

-- formula tronco

areaLateralTronco raioBase raioSeccao altura = pi*(raioBase + raioSeccao)*(sqrt(altura^2 + raioBase^2 + raioSeccao^2))

areaTotalTronco raioBase raioSeccao altura = pi(raioBase^2 + raioSeccao^2) + areaLateralTronco raioBase raioSeccao altura

volumeTronco raioBase raioSeccao altura = pi*altura*(raioBase^2 + raioSeccao^2 + raioBase*raioSeccao)

--formula esferoide oblato
 
excentricidade semiEixoMaior semiEixoMeno = (sqrt(semiEixoMaior^2 + semiEixoMenor^2))/semiEixoMaior


