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

    USE IO_module

    IMPLICIT NONE

    INTEGER  :: total_particle_number, class_num 
    REAL(KIND=8)  ::  target_time, eps_t  

    SAVE

  CONTAINS

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : initial_setting                                               !
!                                                                              !
!   PURPOSE : Initial setup for specific slice or time                         !
!                                                             2019.02.12 K.Noh !
!                                                                              !
!   class_num  =  0 : all data                                                 !
!                                                                              !
!------------------------------------------------------------------------------!
  SUBROUTINE initial_setting

      IMPLICIT NONE

      class_num = 0

      total_particle_number = 0

      target_time  =  97200.0
      eps_t        =  10.0

      dir_name   = "/home/km109/PALM_60/JOBS/NP_test/OUTPUT/LAT_40_H0_200_TIME_12DAY/NP_test_prt_dat.021/"

  END SUBROUTINE initial_setting
  
!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : particle_count                                                !
!                                                                              !
!   PURPOSE : Count the total the number of particles of total simulation      !
!                                                             2019.02.13 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
  SUBROUTINE particle_count(simulated_time,number_of_particles)

      IMPLICIT NONE

      INTEGER,INTENT(IN)  ::  number_of_particles
      REAL(KIND=8),INTENT(IN)  ::  simulated_time

      IF ( (simulated_time - target_time) < eps_t ) THEN 
          total_particle_number = total_particle_number + number_of_particles
      END IF 

  END SUBROUTINE particle_count

END MODULE particle_combine_module
