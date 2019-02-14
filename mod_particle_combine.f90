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
    REAL(KIND=8)  ::  target_time, eps_t, target_z, eps_z, target_x, eps_x

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
!                 1 : xy slice                                                 !
!                 2 : yz slice                                                 !
!                                                                              !
!------------------------------------------------------------------------------!
  SUBROUTINE initial_setting

      IMPLICIT NONE

      class_num = 0

      total_particle_number = 0

      !<  (class_num = 0) CASE
      !<  Target time, which want to extract specific time 
      !<  If you want to extract all time-step, set esp_t as big as possible 
      target_time  =  97200.0
      eps_t        =  1000000.0

      !<  (class_num = 1) CASE
      !<  Target z point, which want to extract speicifc horizontal xy slice
      target_z     = -10.0 
      eps_z        = 2.5

      !<  (class_num = 2) CASE
      !<  Target x point, which want to extract speicifc vertical yz slice
      target_x     = 150.0 
      eps_x        = 2.5

      !<  Directory where the PALM simulated particle data exist
      dir_name   = "/home/km109/PALM_60/JOBS/NP_test/OUTPUT/LAT_40_H0_200_TIME_12DAY/NP_test_prt_dat.021/"

  END SUBROUTINE initial_setting
  
!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : write_header                                                  !
!                                                                              !
!   PURPOSE : write the header of each output files                            !
!                                                                              !
!                                                             2019.02.13 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
  SUBROUTINE write_header(simulated_time,i_proc)

      IMPLICIT NONE

      INTEGER,INTENT(IN)  ::  i_proc
      REAL(KIND=8),INTENT(IN)  ::  simulated_time
      
      CHARACTER(LEN=200)  ::  time_num, file_name, slice_num

      IF ( abs(simulated_time - target_time) < eps_t ) THEN 
          WRITE(time_num,"(I7.7)") INT(simulated_time)
          SELECT CASE (class_num)
              !<  Full 3D case header
              CASE(0) 
                file_name = TRIM(dir_name)//"RESULT/particle_3d_time_"          &
                                          //TRIM(time_num)//".plt"
                OPEN(100,FILE=TRIM(file_name),FORM='FORMATTED',POSITION='APPEND')

                IF(i_proc == 0) WRITE(100,*) 'VARIABLES = X,Y,Z'
                IF(i_proc == 0) WRITE(100,*) 'ZONE'
                IF(i_proc == 0) WRITE(100,*) 'SOLUTIONTIME =',simulated_time

              !<  xy slice case header
              CASE(1)
                WRITE(slice_num,"(I4.3)") INT(target_z)
                file_name = TRIM(dir_name)//"RESULT/particle_xy_"//             &
                            TRIM(slice_num)//"_time_"//TRIM(time_num)//".plt"
                OPEN(100,FILE=TRIM(file_name),FORM='FORMATTED',POSITION='APPEND')

                IF(i_proc == 0) WRITE(100,*) 'VARIABLES = X,Y,PHY'
                IF(i_proc == 0) WRITE(100,*) 'ZONE'
                IF(i_proc == 0) WRITE(100,*) 'SOLUTIONTIME =',simulated_time

              !<  yz slice case header
              CASE(2)
                WRITE(slice_num,"(I3.3)") INT(target_x)
                file_name = TRIM(dir_name)//"RESULT/particle_yz_"//             &
                            TRIM(slice_num)//"_time_"//TRIM(time_num)//".plt"
                OPEN(100,FILE=TRIM(file_name),FORM='FORMATTED',POSITION='APPEND')

                IF(i_proc == 0) WRITE(100,*) 'VARIABLES = Y,Z,PHY'
                IF(i_proc == 0) WRITE(100,*) 'ZONE'
                IF(i_proc == 0) WRITE(100,*) 'SOLUTIONTIME =',simulated_time
          END SELECT
      END IF 

  END SUBROUTINE write_header

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : particle_count                                                !
!                                                                              !
!   PURPOSE : Count the total the number of particles of total simulation      !
!                                                                              !
!                                                             2019.02.13 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
  SUBROUTINE particle_count(simulated_time,number_of_particles)

      IMPLICIT NONE

      INTEGER,INTENT(IN)  ::  number_of_particles
      REAL(KIND=8),INTENT(IN)  ::  simulated_time

      IF ( abs(simulated_time - target_time) < 10.0 ) THEN 
          total_particle_number = total_particle_number + number_of_particles
      END IF 

  END SUBROUTINE particle_count

END MODULE particle_combine_module
