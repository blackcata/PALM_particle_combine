!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : main_PDF.f90                                                     !
!                                                                              !
!   PURPOSE : To make PDF functions of each particle properties                !
!                                                                              !
!                                                             2019.09.21 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

        PROGRAM main_PDF 
            USE IO_module
            USE particle_PDF_module

            IMPLICIT NONE
            
            dir_name = "./"
            CALL FOLDER_SETUP
          
            CALL PDF_init_setting

            file_1_name  = "particle_3d_time_1036649.plt"
            CALL read_par_data(file_1_name)

        END PROGRAM 
