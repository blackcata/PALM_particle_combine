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

            CHARACTER(LEN=200)  ::  data_path

            SAVE 

            CONTAINS

            SUBROUTINE PDF_init_setting
                IMPLICIT NONE

                data_path  =  "./DATA/BC_SGS_Q0_1.0_U_0.01_LAT_40_H0_120_TIME_12DAY/"

            END SUBROUTINE PDF_init_setting 

            SUBROUTINE read_par_data(file_name)
                IMPLICIT NONE
                CHARACTER(LEN=200),INTENT(IN)  :: file_name

                dir_name   =  data_path 
                path_name  =  TRIM(dir_name)//TRIM(file_name)
                
                PRINT*, dir_name
                PRINT*, file_name
                PRINT*, path_name

            END SUBROUTINE read_par_data

        END MODULE particle_PDF_module
