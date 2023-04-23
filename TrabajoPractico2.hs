data Auto = UnAuto {marca::String, modelo:: Float, kilometraje:: Float} deriving (Show, Eq)
data Persona = UnaPersona {nombre::String, impuestos::Bool, autos::[Auto]}

ferrari,fitito,reno,toyota :: Auto   --fitito reno y ferrari son autos importados
ferrari = UnAuto "ferrari" 1990 100
fitito = UnAuto "fiat" 1960 1000000
reno = UnAuto "renault" 2023 0
toyota = UnAuto "toyota" 2010 10000

jorge,martin,juan,matias :: Persona
jorge = UnaPersona "Jorge" True [ferrari,reno,toyota]
martin = UnaPersona "Martin" False [fitito]
juan = UnaPersona "Juan" True [toyota,fitito]
matias = UnaPersona "Matias" False []

honesto:: Persona -> Bool     --devuleve True o False dependiendo si la persona paga sus impuestos o no
honesto persona = impuestos persona == True

calculoValor :: Float -> Float -> Float       --calcula el valor del auto dependiendo si el auto viejo o no
calculoValor modelo kilometraje
    | modelo > 1990 = 2000 * modelo - kilometraje
    | otherwise = 700 * modelo - kilometraje

valorAuto:: Auto -> Float
valorAuto auto       --calcula el valor del auto dependiendo si el auto es importado o no
    | auto == ferrari  || auto == fitito || auto == reno = 2 * calculoValor (modelo auto) (kilometraje auto)
    | otherwise = calculoValor (modelo auto) (kilometraje auto)

sumar::[Float]->Float   --suma todos los elementos de una lista y devuelve el resultado
sumar [ ] = 0
sumar (x:y) = x + sumar (y)

millonario :: Persona -> Bool      --verifica que la suma del valor de todos sus autos sea mayor a 1.000.000
millonario persona = sumar (map valorAuto (autos persona)) > 1000000
