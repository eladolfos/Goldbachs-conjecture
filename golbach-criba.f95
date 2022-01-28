! 23/03/2020
! golbach-criba.f95
! Elser Lopez (eladolfos@gmail.com)
!----------------------------------------
!    Programa busqueda de numeros primos:
!    Este programa utiliza la criba de eratostenes para encontrar los numeros primos en una lista desde 2 hasta n
!----------------------------------------
!
!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Xubuntu Linux) 7.4.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran  -Wall -pedantic -std=f95 -o golbach-criba.e golbach-criba.f95
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
!
!    1. INICIO
     PROGRAM criba
          IMPLICIT NONE
!    2. Definir: VN, VT, VT1 como vectores con entradas enteras de dimensión $n-1$ 
          integer,  ALLOCATABLE:: VN(:), VT(:), VT1(:) ! VN Lista con numeros hasta n, VT lista temporal, VP  lista con primos
!    3. Definir: $n, i, l, k$, como variables enteras para iteraci\'on 
          integer :: n, i, j, k, l, a !numeros para interacion
!    4. Leer: el valor de $n$ del archivo de texto. Parar: en caso de error.
          OPEN(unit=1,File='primos.num', status='unknown') !LEE EL NUMERO ENTRE EL QUE SE CALCULARAN LOS PRIMOS
          READ(1,*) n
          CLOSE(1)
          IF (n<2) THEN
               WRITE(*,*) 'ERROR: Por favor introduza un numero ENTERO mayor o igual a 2'
               STOP
          END IF
          WRITE(*,*) 'Numeros primos entre 2 y', n
          !crea el vector en la memoria temporal
          allocate(VN(n-1))
          !CREA UN VECTOR TEMPORAL
          allocate(VT(n-1))
          allocate(VT1(n-1))
          !allocate(VP(n-1))
          !crea un vector de dimension (n-1) con numeros desde 2 hasta n
          VN=(/(i, i=2,n) /)
                    !write(*,*) VN
          
!    5. Asignar: VT1(1)=2
          VT1(1)=2
          VT=0
          VT1=0
!    6. Hacer: desde $i=2$ hasta $(n-1)$
          DO i=2, (n-1)
               IF (VN(i-1)**2<=n) THEN !calcula los multiplos de un numero en la lista
                    DO j=1, n-1
                         a=VN(i-1)*(j+1)
                         IF (a<=n) THEN !calcula los multiplos de  un numero menores a n
                              VT(j)=a
                              !WRITE(*,*) VT(j)
                         ELSE
                              EXIT
                         END IF
                    END DO
                    !vamos a restar los multiplos guardados en VT de la lista orginal VN
                    VT1(1)=2
                    DO k=1, n-1
                     DO l=1, n-1
                         !WRITE(*,*) VN(k+1), VT(l) 
                         IF (VN(k+1)==VT(l)) THEN !condicion para borrar el numero que no es primo
                             VT1(K+1)=VN(k+1)-VT(l)
                             VN(k+1)=VT1(k+1)
                         ELSE 
                              VT1(k+1)=VN(k+1)
                         END IF
                     END DO
                    END DO
               ELSE
                    EXIT
               END IF
          END DO     

!    7.  Escribiri: en un \textsc{archivo} componentes mayores a cero de VT1.
          OPEN(unit=2,File='numeros-primos.dat', status='unknown') !escritura de los numeros primos en un archivo de texto    
          IF (n<4) THEN
               DO i=1, n-1
                    WRITE(2,*) VN(i)
               END DO
          ELSE
          
          
          DO i=1, n-1
               IF (VT1(i)>0) THEN
                    WRITE(2,*) VT1(i)
               END IF
          END DO

          END IF
          CLOSE(2)
          DEALLOCATE(VN)
          DEALLOCATE(VT)
          DEALLOCATE(VT1)
!    8. FIN
     END PROGRAM criba
     
