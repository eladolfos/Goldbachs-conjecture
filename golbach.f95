! 23/03/2020
! golbach.f95
! Elser Lopez (eladolfos@gmail.com)
!----------------------------------------
!    Programa Conjetura de Golbach:
!    Este programa comprueba la conjetura de golbach en su version fuerete y debil para numeros primos entre  2  y n
!----------------------------------------
!
!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Xubuntu Linux) 7.5.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran  -Wall -pedantic -std=f95 -o golbach.e golbach.f95
!    Copyright (C) 2020
!    E.A. López
!    eladolfos@gmail.com
!
!    This program is free software: you can redistribute it and/or
!    modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of
!    the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!    General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see
!    <http://www.gnu.org/licenses/>.
!
!    1. INICIO
     PROGRAM golbach
          IMPLICIT NONE
!    2. Definir: P1, como un vector con entradas enteras de dimensión $m$ 
          integer,  ALLOCATABLE:: P1(:)  !listas con primos
!    3. Definir: variables axiliares y del problema
          integer :: i, j, k, n, num, m, counter, sum2, sum3, l
          real :: counter3, counter2
!    4. Leer: el valor de n del archivo de texto
          OPEN(unit=2,File='par.num', status='OLD')                        
          READ(2,*) l
          n=l
          CLOSE(2)
          counter=0
!    5. Leer: la lista de números primos desde $2$ hasta un $m$ cercano a $n$.
          OPEN(unit=1,File='numeros-primos.dat', status='OLD')
          !cuenta los numeros primos que hay entre 2 y n
          DO i=1, n
               READ(1,*) num
               IF (num<=n) THEN
               counter=counter+1
               ELSE
                    EXIT
               END IF
          END DO
          CLOSE(1)
          !carga los numeros primos menores a n y crea una lista de longitud m donde m<n
          OPEN(unit=1,File='numeros-primos.dat', status='OLD')
               m=counter
               allocate(P1(m))
               READ(1,*) P1
          CLOSE(1)
!    6. Hacer: desde i=1 hasta m
          counter2=0
          sum2=0
          DO i=1, m
               DO j=1, m
                  sum2=P1(i)+P1(j)
                  !WRITE(*,*) P1(i), P2(j), sum2
                  IF (sum2==n) THEN
                         IF(i==j) THEN
                         counter2=counter2+1
                         ELSE
                              counter2=counter2+1.0/2.0
                         END IF
                         !WRITE(*,*) counter2
                   END IF
               END DO
          END DO

!    7. Hacer: desde i=1 hasta m
          ! comprueba cuales numeros pares mayores que 5 se pueden escribir como suma de tres primos
          counter3=0
          IF (n>5) THEN
               DO i=1, m
                    DO j=1, m
                         DO k=1, m 
                              sum3=P1(i)+P1(j)+P1(k)
                              IF (sum3==n) THEN
                                   !WRITE(*,*) P1(i),P1(j),P1(k), sum3
                                   IF (i==j .and. j==k) THEN
                                        counter3=counter3+1
                                   ELSEIF(i.ne.j .and. j.ne.k) THEN
                                        counter3=counter3+(1.0/6.0)
                                   ELSE
                                        counter3=counter3+(1.0/3.0)
                                   END IF
                              END IF
                         END DO
                    END DO
               END DO
          END IF
                              
                                   
!    8. Escribir: en pantalla 
          WRITE(*,*) 'CANTIDAD DE FORMA EN LAS QUE', n, 'PUEDE ESCRIBIRSE COMO' 
          WRITE(*,*)  'SUMA DE DOS PRIMOS--> ', NINT(counter2), 'SUMA DE TRES PRIMOS-->', NINT(counter3)      
          DEALLOCATE(P1)          
!    9. FIN
     END PROGRAM golbach
