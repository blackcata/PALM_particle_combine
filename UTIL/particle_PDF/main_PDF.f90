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
            CALL FOLDER_SETUP
          
            CALL PDF_init_setting

            dir_name     =  data_path 
            file_2_name  =  "file_name_list.dat" 
            path_name    =  TRIM(dir_name)//TRIM(file_2_name)

            OPEN(85, FILE=path_name, FORM='FORMATTED')

            !--Read each particle id & position & concentration 
            DO it = 1,N_par
                READ(85,*,IOSTAT=iostatus) file_1_name
                IF (iostatus < 0) EXIT
                Nt = Nt + 1
            END DO 
            CLOSE(85)

            ALLOCATE( par_id(1:N_par,1:Nt), x(1:N_par,1:Nt), y(1:N_par,1:Nt) )
            ALLOCATE( z(1:N_par,1:Nt), chl(1:N_par,1:Nt) )

            OPEN(85, FILE=path_name, FORM='FORMATTED')
            ALLOCATE(file_name_list(1:Nt))
            DO it = 1,Nt
                READ(85,*,IOSTAT=iostatus) file_name_list(it)
            END DO 
                
            DO it = ind_str,ind_end
                file_1_name  =  file_name_list(it)
                CALL read_par_data(file_1_name)
            END DO 

            DEALLOCATE(par_id, x, y, z, chl)

        END PROGRAM 
