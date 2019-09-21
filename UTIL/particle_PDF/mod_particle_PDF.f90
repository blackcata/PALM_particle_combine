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

            INTEGER             ::  N_par
            CHARACTER(LEN=200)  ::  data_path

            REAL(KIND=8),DIMENSION(:),ALLOCATABLE  :: par_id, x, y, z, chl

            SAVE 

            CONTAINS

            SUBROUTINE PDF_init_setting
                IMPLICIT NONE

                N_par      =  90000
                data_path  =  "./DATA/BC_SGS_Q0_1.0_U_0.01_LAT_40_H0_120_TIME_12DAY/"

                ALLOCATE( par_id(1:N_par), x(1:N_par), y(1:N_par), z(1:N_par) ) 
                ALLOCATE( chl(1:N_par) )

            END SUBROUTINE PDF_init_setting 

            SUBROUTINE read_par_data(file_name)
                IMPLICIT NONE
                CHARACTER(LEN=200),INTENT(IN)  :: file_name

                INTEGER   :: it 
                CHARACTER(LEN=200)             :: tmp_header

                dir_name   =  data_path 
                path_name  =  TRIM(dir_name)//TRIM(file_name)
                
                OPEN(85, FILE=path_name, FORM='FORMATTED')
                !--Read headers of each file 
                DO it = 1,3
                    READ(85,*)  tmp_header
                    PRINT*, tmp_header
                END DO 

                DO it = 1,N_par
                    READ(85,*) par_id(it), x(it), y(it), z(it), chl(it)
                END DO 

            END SUBROUTINE read_par_data

        END MODULE particle_PDF_module
