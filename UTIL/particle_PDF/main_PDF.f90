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
            INTEGER  ::  N_PDF
            REAL(KIND=8)  :: PDF_min, PDF_max
            REAL(KIND=8),DIMENSION(:),ALLOCATABLE  :: PDF

            dir_name = "./"
            CALL FOLDER_SETUP
          
            CALL PDF_init_setting

            !-------------------------------------------------------------------!
            !                     READING EACH PARTICLE FILE                    !
            !-------------------------------------------------------------------!
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

            OPEN(85, FILE=path_name, FORM='FORMATTED')
            ALLOCATE(file_name_list(1:Nt))
            DO it = 1,Nt
                READ(85,*,IOSTAT=iostatus) file_name_list(it)
            END DO 
            CLOSE(85)
              
            !--Read total particle ID number
            dir_name     =  "./DATA/"
            file_2_name  =  "particle_ID.dat" 
            path_name    =  TRIM(dir_name)//TRIM(file_2_name)
            ALLOCATE( par_id(1:N_par) ) 

            OPEN(100, FILE=path_name, FORM='FORMATTED')
            DO it = 1,N_par
                READ(100,*) par_id(it)
            END DO 
            CLOSE(100) 

            !--Read part of particle files
            Nt_loc  =  (ind_end - ind_str) + 1     
            ALLOCATE( z(1:N_par,1:Nt_loc), chl(1:N_par,1:Nt_loc) )
            DO it = ind_str,ind_end
                file_1_name  =  file_name_list(it)
                CALL read_par_data(file_1_name)
                PRINT*,TRIM(file_name_list(it))
            END DO 
            
            CALL calc_CEA(z)
            CALL calc_NVG(z)

            !-------------------------------------------------------------------!
            !                       CALCULATING EACH PDF                        !
            !-------------------------------------------------------------------!
            !--Calculate the PDF of Z poisition of each particle
            PDF_min    =  -100.0
            PDF_max    =  0.0
            N_PDF      =  101
            dir_name   =  "./RESULT/" ; file_1_name  =  "PDF_Z.dat" 

            ALLOCATE( tmp_1d(1:SIZE(z)), PDF(1:N_PDF) )
                CALL convert_2D_1D(z,tmp_1D)
                CALL calc_PDF(tmp_1D,PDF,PDF_min,PDF_max,N_PDF)
                CALL write_PDF_data(file_1_name,PDF,PDF_min,PDF_max)
            DEALLOCATE( tmp_1d, PDF )

            !--Calculate the PDF of CHL Concentration of each particle
            PDF_min    =  0.0
            PDF_max    =  3.0e-9
            N_PDF      =  101
            dir_name   =  "./RESULT/" ; file_1_name  =  "PDF_CHL.dat" 

            ALLOCATE( tmp_1d(1:SIZE(z)), PDF(1:N_PDF) )
                CALL convert_2D_1D(chl,tmp_1D)
                CALL calc_PDF(tmp_1D,PDF,PDF_min,PDF_max,N_PDF)
                CALL write_PDF_data(file_1_name,PDF,PDF_min,PDF_max)
            DEALLOCATE( tmp_1d, PDF )

            !-------------------------------------------------------------------!
            !                       WRITING OUTPUT FILES                        !
            !-------------------------------------------------------------------!
            !--Write the total Z & CHL time series
            file_1_name  =  "Total_Z.dat"
            CALL write_par_data(file_1_name,z,"F")

            file_1_name  =  "Total_CHL.dat"
            CALL write_par_data(file_1_name,chl,"E")

            DEALLOCATE(par_id, z, chl, CEA)

        END PROGRAM 
