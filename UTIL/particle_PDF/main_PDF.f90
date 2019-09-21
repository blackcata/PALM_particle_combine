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
            
            INTEGER  ::  it, iostatus

            dir_name = "./"
            call folder_setup
          
            CALL PDF_init_setting

            dir_name     =  data_path 
            file_2_name  =  "file_name_list.dat" 
            path_name    =  TRIM(dir_name)//TRIM(file_2_name)

            OPEN(85, FILE=path_name, FORM='FORMATTED')

            !--Read each particle id & position & concentration 
            DO it = 1,N_par
                READ(85,*,IOSTAT=iostatus) file_1_name
                path_name    =  TRIM(dir_name)//TRIM(file_1_name)
                IF (iostatus < 0) EXIT

                Nt = Nt + 1
                CALL read_par_data(file_1_name)

            END DO 


        END PROGRAM 
