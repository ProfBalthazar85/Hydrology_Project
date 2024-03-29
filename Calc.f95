
!  --------------------------------------------------
!  Class Project, Research and Teaching Methods 
!  --------------------------------------------------
PROGRAM CMEAN
        REAL :: MOS, PUL, YIJ, AMT_MAX, ADR_Intsity, AMT_Min
        REAL :: SUM_Prcp, SUM_Tmax, SUM_Tmin, AVG_Prcp, AM_Prcp, AND_Rainy, PD_Wet
        INTEGER :: Reason, ROW, J, M, N, YAR, FILE_END, FILE_END, NMONTH, DAY, Z, MONTH
        DIMENSION MOS(100000, 6), PUL(100000, 6), SUM_Prcp(12, 10000), SUM_Tmax(12, 10000) 
        DIMENSION DAY(12, 10000), SUM_Tmin(12, 10000), YIJ (12, 10000)
        DIMENSION MP (12), ADR_Intsity(12), PD_Wet(12), AND_Rainy (12), AMT_MAX(12)
        DIMENSION AM_Prcp(12), AMT_Min(12)
        !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        !Set initial values zero
        SUM_Prcp = 0
        YIJ = 0
        SUM_Tmax = 0
        SUM_Tmin = 0
        AM_Prcp = 0
        APrcp = 0        
        !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
             
        ! Correcting corrupt data
       
        !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        FILE_END = 30000 ! End of file
        YEARS_N = 67 ! Time series length(Year) (2007 - 1941 + 1)
        
        OPEN (1, FILE = 'PULLMAN.TXT', STATUS = 'UNKNOWN')
        DO ROW = 1, FILE_END            
            READ (1, *,IOSTAT=Reason) PUL(ROW,1), PUL(ROW,2), PUL(ROW,3), PUL(ROW,4), PUL(ROW,5), PUL(ROW,6)
            IF (Reason > 0)  THEN
                PRINT*, 'something wrong ...'
            ELSE IF (Reason < 0) THEN
                PRINT*, 'end of file reached ...', ROW , ' Days'
                EXIT
            ELSE
                !do normal stuff ...'
            END IF
        END DO
        CLOSE (1)
         
        OPEN (2, FILE = 'MOSCOW.TXT', STATUS = 'OLD')        
        DO ROW = 1, FILE_END
            READ (2, *,IOSTAT=Reason) MOS(ROW,1), MOS(ROW,2), MOS(ROW,3), MOS(ROW,4), MOS(ROW,5), MOS(ROW,6)
            IF (Reason > 0)  THEN
                PRINT*, 'something wrong ...'
            ELSE IF (Reason < 0) THEN
                PRINT*, 'end of file reached ...'
                FILE_END = ROW - 2
                EXIT
            ELSE
                !do normal stuff ...'
            END IF
            IF (MOS(ROW,2)== MOS(ROW-1,2)) THEN
                ! Nothing
            ELSE                        
                DO J = L, ROW-1
                    IF (MOS(J,4)> 300) THEN
                        DO Z = 1, FILE_END
                            IF (MOS(J,1) == PUL(Z,1)) THEN
                                IF (MOS(J,2) == PUL(Z,2)) THEN    
                                    MOS(J,4)= PUL(Z,4)
                                END IF
                            END IF
                        END DO                            
                    END IF
                    IF (MOS(J,5)> 300) THEN 
                        DO Z = 1, FILE_END
                            IF (MOS(J,1) == PUL(Z,1)) THEN
                                IF (MOS(J,2) == PUL(Z,2)) THEN    
                                    MOS(J,5)= PUL(Z,5)
                                END IF
                            END IF
                        END DO 
                    END IF
                    IF (MOS(J,6)> 10) THEN 
                        DO Z = 1, FILE_END
                            IF (MOS(J,1) == PUL(Z,1)) THEN
                                IF (MOS(J,2) == PUL(Z,2)) THEN    
                                    MOS(J,6)= PUL(Z,6)
                                END IF
                            END IF
                        END DO 
                    END IF                      
                END DO
                L=I                                         
            END IF  
        END DO 
        CLOSE (2)
        
        
        OPEN (3, FILE = 'MOS2.TXT', STATUS='UNKNOWN')
        DO ROW = 1, FILE_END
            WRITE (3,*)  MOS(ROW,1), MOS(ROW,2), MOS(ROW,3), MOS(ROW,4), MOS(ROW,5), MOS(ROW,6)
        END DO
        CLOSE (3) 
        
        !PRINT*, 'end of corrections ...'
        
        !######################################################################### 
            
        ! Calculations
        
        !########################################################################
        OPEN (4, FILE = 'MOS2.TXT', STATUS = 'UNKNOWN')        
        READ (4, *,IOSTAT=Reason) MOS(1,1), MOS(1,2), MOS(1,3), MOS(1,4), MOS(1,5), MOS(1,6)
        DO ROW = 2, FILE_END
            READ (4, *,IOSTAT=Reason) MOS(ROW,1), MOS(ROW,2), MOS(ROW,3), MOS(ROW,4), MOS(ROW,5), MOS(ROW,6)
            DO MONTH = 1, 12    
                DO YAR = 1, YEARS_N
                    IF (MOS(ROW,2) == MONTH) THEN                
                        IF (MOS(ROW,1) == 1940 + YAR) THEN
                            DAY(MONTH, 1940 + YAR) = MOS(ROW,3)                                                                                                                                                             
 
                            ! The average monthly precipitation
                            SUM_Prcp (MONTH, 1940 + YAR) = MOS(ROW,6)+  SUM_Prcp(MONTH, 1940  + YAR) 
                            ! The average number of rainy days in a month
                            IF (MOS(ROW,6)> 0) THEN
                                YIJ (MONTH, 1940 + YAR) = YIJ (MONTH, 1940 + YAR) + 1
                            END IF
                            ! The average daily rainfall intensity for a month
                            ! The probability of a wet day for a specific month 
                            ! The monthly average maximum 
                            SUM_Tmax (MONTH, 1940 + YAR) = MOS(ROW,4)+  SUM_Tmax(MONTH, 1940  + YAR) 
                            ! The monthly average minimum 
                            SUM_Tmin (MONTH, 1940 + YAR) = MOS(ROW,5)+  SUM_Tmin(MONTH, 1940  + YAR) 
                        END IF
                    END IF
                END DO 
            END DO            
        END DO 
        CLOSE(4)

        
        DO MONTH = 1, 12
            DO YAR = 1, YEARS_N
                ! Average monthly precipitation
                AM_Prcp(MONTH) = SUM_Prcp (MONTH, 1940 + YAR) + AM_Prcp(MONTH)
                !The average number of rainy days in a month
                AND_Rainy(MONTH) = YIJ(MONTH, 1940 + YAR) + AND_Rainy(MONTH)
                ! The average daily rainfall intensity for a month
                ! The probability of a wet day for a specific month
                ! The monthly average maximum TEMP
                IF (DAY(MONTH, 1940  + YAR) == 0) THEN
                    !PRINT*, K, 1940 + R
                END IF 
                SUM_Tmax (MONTH, 1940 + YAR) = (SUM_Tmax (MONTH, 1940  + YAR) )/(DAY(MONTH, 1940  + YAR) )
                AMT_MAX(MONTH)= SUM_Tmax (MONTH, 1940 + YAR) + AMT_MAX(MONTH)
                ! The monthly average minimum TEMP
                SUM_Tmin (MONTH, 1940 + YAR) = (SUM_Tmin (MONTH, 1940  + YAR) )/(DAY(MONTH, 1940  + YAR) )
                AMT_MIN(MONTH)= SUM_Tmin (MONTH, 1940 + YAR) + AMT_MIN(MONTH)                
            END DO
            ! Average monthly precipitation
            AM_Prcp(MONTH) = AM_Prcp(MONTH)/YEARS_N
            ! The average number of rainy days in a month
            AND_Rainy(MONTH) = AND_Rainy(MONTH)/YEARS_N
            ! The average daily rainfall intensity for a month
            ADR_Intsity(MONTH) = AM_Prcp(MONTH)/AND_Rainy(MONTH)
            ! The probability of a wet day for a specific month
            PD_Wet(MONTH) = AND_Rainy(MONTH)/30
            ! The monthly average maximum TEMP
            AMT_MAX(MONTH) = AMT_MAX(MONTH)/YEARS_N
            ! The monthly average minimum TEMP
            AMT_MIN(MONTH) = AMT_MIN(MONTH)/YEARS_N 
            ! Annual precipitation
            APrcp = AM_Prcp(MONTH) + APrcp               
        END DO
       
                   
        ! Save output data
        OPEN (5, FILE = 'RESULT.TXT', STATUS='UNKNOWN')
        WRITE (5,*)   
        DO MONTH = 1, 12
            WRITE (5,*)  MONTH , AM_Prcp(MONTH), AND_Rainy(MONTH), ADR_Intsity(MONTH), PD_Wet(MONTH), AMT_MAX(MONTH), AMT_MIN(MONTH)
        END DO
        WRITE (5,*) 'Annual Precipitation', APrcp
        CLOSE(5)
        
        !PRINT*, 'end of calculations ...'
      
End