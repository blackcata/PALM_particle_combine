!------------------------------------------------------------------------------!
!   PROGRAM : mod_particle_PDF.f90                                             !
!                                                                              !
!   PURPOSE : Calculate particle propertie PDF(Probabillity Density Function)  !
!                                                                              !
!                                                             2019.09.21.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        MODULE particle_PDF_module
            USE IO_module 

            IMPLICIT NONE

            INTEGER             ::  N_par, Nt, Nt_loc, ind_str, ind_end, ind_t
            REAL(KIND=8)        ::  D, GN, gam, h_e, dt, N_hour
            CHARACTER(LEN=200)  ::  data_path, result_path

            INTEGER(KIND=8),DIMENSION(:),ALLOCATABLE  :: par_id
            REAL(KIND=8),DIMENSION(:),ALLOCATABLE    :: tmp_1D, CEA, NVG
            REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE  :: z, chl, dCHL

            SAVE 

            CONTAINS

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : PDF_initial_setting                                           !
!                                                                              !
!   PURPOSE : Initial setup for PDF Calcuation                                 !
!                                                             2019.09.21 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
            SUBROUTINE PDF_init_setting
                IMPLICIT NONE

                Nt         =  0
                ind_str    =  578
                ind_end    =  1153
                ind_t      =  0 

                N_par      =  90000
                data_path  =  "./DATA/BC_LAT_40_H0_120_TIME_12DAY/"

                D    =   0.1 ! Plankton Death Rate  [day^-1]
                GN   =   1.0 ! Plankton Growth rate * Nutrient [day^-1]
                gam  =   0.1 ! Light Attenuation Coefficient [m^-1]
                h_e  =  -1.0 / gam * log(D/GN) ! Critical Depth[m]

                dt   =  150.0 ! Time step of particle output [s]

                result_path   =  "./RESULT/"

                ALLOCATE( CEA(1:N_par), NVG(1:N_par) ) 

            END SUBROUTINE PDF_init_setting 

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : read_par_data                                                 !
!                                                                              !
!   PURPOSE : Read particle datas properties                                   !
!                                                             2019.09.21 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

            SUBROUTINE read_par_data(file_name)
                IMPLICIT NONE
                INTRINSIC  :: findloc
                CHARACTER(LEN=200),INTENT(IN)  :: file_name

                INTEGER             ::  it, ind_ID(1)
                INTEGER(KIND=8)     ::  tmp_id
                REAL(KIND=8)        ::  tmp_1, tmp_2, tmp_z, tmp_chl
                CHARACTER(LEN=200)  ::  tmp_header

                ind_t      =  ind_t + 1 
                dir_name   =  data_path 
                path_name  =  TRIM(dir_name)//TRIM(file_name)
                
                OPEN(85, FILE=path_name, FORM='FORMATTED')
                !--Read headers of each file 
                DO it = 1,3
                    READ(85,*)  tmp_header
                END DO 

                !--Read each particle id & position & concentration 
                DO it = 1,N_par
                    READ(85,*) tmp_id, tmp_1, tmp_2, tmp_z, tmp_chl
                    ind_ID  = FINDLOC(par_id,VALUE=tmp_id) 
                    z(ind_ID,ind_t)    =  tmp_z
                    chl(ind_ID,ind_t)  =  tmp_chl
                END DO 
                CLOSE(85)

            END SUBROUTINE read_par_data

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : convert_2D_1D                                                 !
!                                                                              !
!   PURPOSE : Convert 2D arrays to 1D arrays                                   !
!                                                             2019.09.23 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
            SUBROUTINE convert_2D_1D(input_2D,output_1D)
                IMPLICIT NONE

                REAL(KIND=8),DIMENSION(:,:),INTENT(IN)  ::  input_2D
                REAL(KIND=8),DIMENSION(:),INTENT(OUT)   ::  output_1D

                INTEGER :: it, N_1D, ind_str, ind_end
                INTEGER,DIMENSION(2)  :: N_2D

                N_2D  =  SHAPE(input_2D)
                N_1D  =  SIZE(output_1D)
                
                DO it = 1,N_2D(2) 
                    ind_str  =  (it-1) * N_2D(1) + 1
                    ind_end  =  (it) * N_2D(1)
                    output_1D(ind_str:ind_end)  =  input_2D(:,it)
                END DO 

            END SUBROUTINE convert_2D_1D

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : calc_CEA                                                      !
!                                                                              !
!   PURPOSE : Calculate the Cumulative Euphotic Age                            !
!                                                             2019.09.23 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
            SUBROUTINE calc_CEA(z_tot)
                IMPLICIT NONE

                INTEGER :: it, par 
                REAL(KIND=8),DIMENSION(:,:),INTENT(IN)  :: z_tot

                CEA(1:N_par)  =  0.0
                DO par = 1,N_par
                    DO it = 1,Nt_loc
                        IF (z_tot(par,it) > -h_e) CEA(par)  =  CEA(par) + 1 
                    END DO 
                END DO 

                CEA(1:N_par)  =  CEA(1:N_par) / Nt_loc

            END SUBROUTINE calc_CEA

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : calc_NVG                                                      !
!                                                                              !
!   PURPOSE : Calculate the Number of Vertical Grids Visited                   !
!                                                             2019.09.23 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
            SUBROUTINE calc_NVG(z_tot)
                IMPLICIT NONE

                INTEGER :: it, par 
                REAL(KIND=8),DIMENSION(:,:),INTENT(IN)  :: z_tot

                N_hour  = Nt_loc * dt / 3600. ! [hour]

                NVG(1:N_par)  =  0.0
                DO par = 1,N_par
                    DO it = 2,Nt_loc
                        NVG(par)  = NVG(par) +                                  &
                                    ABS(FLOOR(z_tot(par,it) - z_tot(par,it-1)))
                    END DO 
                END DO 

                NVG(1:N_par)  =  NVG(1:N_par) / N_hour

            END SUBROUTINE calc_NVG

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : calc_dCHL                                                     !
!                                                                              !
!   PURPOSE : Calculate the chlorophyll growth rate                            !
!                                                             2019.09.23 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
            SUBROUTINE calc_dCHL(chl_tot)
                IMPLICIT NONE

                INTEGER :: it, par 
                REAL(KIND=8),DIMENSION(:,:),INTENT(IN)  :: chl_tot

                dCHL(1:N_par,1:Nt_loc-1)  =  0.0
                DO par = 1,N_par
                    DO it = 1,Nt_loc-1
                        dCHL(par,it)  = (chl_tot(par,it+1) - chl_tot(par,it))/dt 
                    END DO 
                END DO 

            END SUBROUTINE calc_dCHL

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : calc_PDF                                                      !
!                                                                              !
!   PURPOSE : Calculate PDF from input 1D arrays                               !
!                                                             2019.09.21 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
            SUBROUTINE calc_PDF(input,PDF,PDF_min,PDF_max,N_PDF)
                IMPLICIT NONE

                INTEGER,INTENT(IN)  :: N_PDF
                REAL(KIND=8),INTENT(IN)  ::  PDF_min, PDF_max
                REAL(KIND=8),DIMENSION(:),INTENT(IN)  ::   input
                REAL(KIND=8),DIMENSION(:),INTENT(OUT)  ::  PDF

                INTEGER  :: it , it_bin, N_input
                REAL(KIND=8)  ::  dPDF, tmp_bin

                dPDF     =  (PDF_max - PDF_min) / (N_PDF-1)
                N_input  =  SIZE(input)
                PDF(1:N_PDF)  = 0.0
                DO it = 1,N_input
                    DO it_bin  =  1,N_PDF
                        tmp_bin  =  PDF_min + dPDF * (it_bin -1)
                        IF ( input(it) > tmp_bin - dPDF*0.5        .AND.        &
                             input(it) < tmp_bin + dPDF*0.5) THEN 
                            PDF(it_bin)  =  PDF(it_bin) + 1 
                            EXIT
                        END IF
                    END DO 
                END DO 

                PDF  =  PDF / N_input 

            END SUBROUTINE calc_PDF

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : write_PDF_data                                                !
!                                                                              !
!   PURPOSE : Write particle datas PDF                                         !
!                                                             2019.09.23 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

            SUBROUTINE write_PDF_data(file_name,PDF,PDF_min,PDF_max)
                IMPLICIT NONE

                REAL(KIND=8),INTENT(IN)  ::  PDF_min, PDF_max
                REAL(KIND=8),DIMENSION(:),INTENT(IN)  ::  PDF
                CHARACTER(LEN=200),INTENT(IN)  :: file_name

                INTEGER   :: it, N_PDF
                REAL(KIND=8)  :: tmp_bin, dPDF

                N_PDF  =  SIZE(PDF)
                dPDF     =  (PDF_max - PDF_min) / (N_PDF-1)

                path_name  =  TRIM(dir_name)//TRIM(file_name)
                OPEN(85, FILE=path_name, FORM='FORMATTED')
                DO it = 1,N_PDF
                    tmp_bin  =  PDF_min + dPDF * (it -1)
                    WRITE(85,"(E18.10,F18.10)") tmp_bin, PDF(it)
                END DO 
                CLOSE(85)

            END SUBROUTINE write_PDF_data

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : write_par_1D_data                                             !
!                                                                              !
!   PURPOSE : Read particle datas properties                                   !
!                                                             2019.09.23 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

            SUBROUTINE write_par_1D_data(file_name,input_1D,write_type)
                IMPLICIT NONE

                REAL(KIND=8),DIMENSION(:),INTENT(IN)  ::  input_1D
                CHARACTER(LEN=1),INTENT(IN)    :: write_type
                CHARACTER(LEN=200),INTENT(IN)  :: file_name

                INTEGER  :: it, N
                CHARACTER(LEN=100)    :: str_Nt, format_1D

                N  =  SIZE(input_1D)

                path_name  =  TRIM(dir_name)//TRIM(file_name)
                format_1D="("//TRIM(write_type)//"18.10)"
                OPEN(85, FILE=path_name, FORM='FORMATTED')
                DO it = 1,N
                    WRITE(85,format_1D) input_1D(it)
                END DO 
                CLOSE(85)

            END SUBROUTINE write_par_1D_data

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : write_par_2D_data                                             !
!                                                                              !
!   PURPOSE : Read particle datas properties                                   !
!                                                             2019.09.23 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

            SUBROUTINE write_par_2D_data(file_name,input_2D,write_type)
                IMPLICIT NONE

                REAL(KIND=8),DIMENSION(:,:),INTENT(IN)  ::  input_2D
                CHARACTER(LEN=1),INTENT(IN)    :: write_type
                CHARACTER(LEN=200),INTENT(IN)  :: file_name

                INTEGER  :: it, N1, N2
                INTEGER,DIMENSION(2)  :: N_2D
                CHARACTER(LEN=100)    :: str_Nt, format_2D

                N_2D  =  SHAPE(input_2D)
                N1    =  N_2D(1)
                N2    =  N_2D(2)

                path_name  =  TRIM(dir_name)//TRIM(file_name)
                WRITE(str_Nt,"(I3)") N2
                format_2D="("//TRIM(str_Nt)//TRIM(write_type)//"18.10)"
                OPEN(85, FILE=path_name, FORM='FORMATTED')
                DO it = 1,N1
                    WRITE(85,format_2D) input_2D(it,:)
                END DO 
                CLOSE(85)

            END SUBROUTINE write_par_2D_data

        END MODULE particle_PDF_module
