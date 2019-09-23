!------------------------------------------------------------------------------!
!                                                                              !
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

            INTEGER             ::  N_par, Nt, ind_str, ind_end, ind_t
            CHARACTER(LEN=200)  ::  data_path

            REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE  :: par_id, z, chl

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
                ind_str    =  1
                ind_end    =  10
                ind_t      =  0 

                N_par      =  90000
                data_path  =  "./DATA/BC_SGS_Q0_1.0_U_0.01_LAT_40_H0_120_TIME_12DAY/"

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
                CHARACTER(LEN=200),INTENT(IN)  :: file_name

                INTEGER   :: it 
                REAL(KIND=8)  :: tmp_1, tmp_2
                CHARACTER(LEN=200)             :: tmp_header

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
                    READ(85,*) par_id(it,ind_t), tmp_1, tmp_2,                  &
                               z(it,ind_t), chl(it,ind_t)
                END DO 

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
                
                DO it = 1,N_2D(1) 
                    ind_str  =  (it-1) * N_2D(2) + 1
                    ind_end  =  (it) * N_2D(2)
                    output_1D(ind_str:ind_end)  =  input_2D(it,:)
                END DO 

            END SUBROUTINE convert_2D_1D

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

                DO it = 1,N_input
                    DO it_bin  =  1,N_PDF
                        tmp_bin  =  PDF_min + dPDF * (it_bin -1)
                        IF ( input(it) > tmp_bin - dPDF*0.5        .AND.        &
                             input(it) < tmp_bin + dPDF*0.5) THEN 
                            PDF(it)  =  PDF(it) + 1 
                            EXIT
                        END IF
                    END DO 
                END DO 

                PDF  =  PDF / N_input 

            END SUBROUTINE calc_PDF

        END MODULE particle_PDF_module
