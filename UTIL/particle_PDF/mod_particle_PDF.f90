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
                ind_end    =  577
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
!   SUBROUTINE : calc_PDF                                                      !
!                                                                              !
!   PURPOSE : Calculate PDF from input 1D arrays                               !
!                                                             2019.09.21 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
            SUBROUTINE calc_PDF
                IMPLICIT NONE
            END SUBROUTINE calc_PDF

        END MODULE particle_PDF_module
