!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : mod_particle_combine.f90                                         !
!                                                                              !
!   PURPOSE : Combine the PALM generated particle binary to each time file     !
!                                                                              !
!                                                             2019.02.12.K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
MODULE particle_combine_module
    IMPLICIT NONE

    SAVE

  CONTAINS

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : initial_setting                                               !
!                                                                              !
!   PURPOSE : Initial setup for specific slice or time                         !
!                                                             2019.02.12 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
  SUBROUTINE initial_setting

      USE IO_module 

      IMPLICIT NONE

  END SUBROUTINE initial_setting
  
END MODULE particle_combine_module
