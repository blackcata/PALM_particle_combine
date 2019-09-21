!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : IO_module.f90                                                    !
!                                                                              !
!   PURPOSE : Write each variables in the RESULT folder.                       !
!                                                                              !
!                                                             2017.03.02 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

          MODULE IO_module

            CHARACTER(LEN=200) :: file_1_name, file_2_name, dir_name, path_name
            CHARACTER(LEN=200),DIMENSION(:),ALLOCATABLE  :: file_name_list

          !--------------------------------------------------------------------!
          !                   Interfaces of IO Subroutines                     !
          !--------------------------------------------------------------------!

            !--------Folder Initializing Subroutine
            INTERFACE FOLDER_SETUP
              MODULE PROCEDURE FOLDER_SETUP
            END INTERFACE FOLDER_SETUP

          CONTAINS

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : FOLDER_SETUP                                                  !
!                                                                              !
!   PURPOSE : Initialization the result folder                                 !
!                                                                              !
!                                                             2017.03.02 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
            SUBROUTINE FOLDER_SETUP
              IMPLICIT NONE

              !------------------------------------------------------------------!
              !                  Make & Initialize Result folder                 !
              !------------------------------------------------------------------!
              path_name = TRIM(dir_name)//"RESULT"
              print*,path_name
              CALL SYSTEM('mkdir  '//TRIM(path_name))
              CALL SYSTEM('rm -rf '//TRIM(path_name)//'/*.plt')

            END SUBROUTINE FOLDER_SETUP
          
          END MODULE IO_module
