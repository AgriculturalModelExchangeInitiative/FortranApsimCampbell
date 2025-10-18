PROGRAM main
   USE Soiltempmod, ONLY: init_soiltemp, model_soiltemp
   IMPLICIT NONE
   integer, parameter :: dp = selected_real_kind(15)
   integer, parameter :: n_days = 10958    ! Adjust to your file length
   REAL(dp) :: tmin(n_days), tmax(n_days), t2m(n_days), rain(n_days), srad(n_days), &
      dayld(n_days), sunup(n_days), sundn(n_days), eoad(n_days), &
      esp(n_days), le(n_days), g(n_days), snow(n_days), es(n_days)
   integer :: i, ios, date(n_days), doy
   character(len=256) :: line, filename
   integer :: unit
   integer, parameter ::  max_layers = 100
   character(len=10)  :: soil_idi, soil_id(max_layers)
   character(len=20)  :: soil_namei, soil_name(max_layers)
   integer            :: slidi, slid(max_layers)
   REAL(dp)               :: sllti, sllbi, thicki, slbdmi, sloci, slsati, slduli, &
      sllli, slclyi, slsili, slsndi, svsei
   REAL(dp)               :: sllt(max_layers), sllb(max_layers), thick(max_layers)
   REAL(dp)               :: slbdm(max_layers), sloc(max_layers), slsat(max_layers)
   REAL(dp)               :: sldul(max_layers), slll(max_layers), slcly(max_layers), &
      SLSIL(max_layers), SLSND(max_layers), svse(max_layers)
   integer ::  n, year, month, day
   character(len=10)  :: ws_id, target_id
   character(len=30)  :: site_name, country, site_namei, countryi
   REAL(dp) :: xlat, xlong, tamp, tav
   REAL(dp) :: xlati, xlongi, tampi, tavi, albedoi, nbLayersi, soilDepthi, meanBdi
   integer :: lai
   character(len=250) :: filepath
   character(len=20)  :: aw_value, lai_char
   integer :: pos1, pos2


   ! Declare variables
   REAL(dp) :: weather_MinT, weather_MaxT, weather_MeanT
   REAL(dp) :: weather_Tav, weather_Amp, weather_AirPressure, weather_Wind
   REAL(dp) :: weather_Latitude, weather_Radn
   INTEGER :: clock_Today_DayOfYear
   REAL(dp) :: microClimate_CanopyHeight,  boundaryLayerConductance, mintempyesterday
   REAL(dp), ALLOCATABLE :: physical_Thickness(:), physical_BD(:)
   REAL(dp) :: ps
   REAL(dp), ALLOCATABLE :: physical_Rocks(:), physical_ParticleSizeSand(:)
   REAL(dp), ALLOCATABLE :: physical_ParticleSizeSilt(:), physical_ParticleSizeClay(:)
   REAL(dp), ALLOCATABLE :: organic_Carbon(:), waterBalance_SW(:)
   REAL(dp) :: waterBalance_Eos, waterBalance_Eo, waterBalance_Es, waterBalance_Salb
   REAL(dp), ALLOCATABLE :: pInitialValues(:)
   REAL(dp) :: DepthToConstantTemperature, timestep, latentHeatOfVapourisation
   REAL(dp) :: stefanBoltzmannConstant
   INTEGER :: airNode, surfaceNode, topsoilNode, numPhantomNodes
   REAL(dp) :: constantBoundaryLayerConductance
   INTEGER :: numIterationsForBoundaryLayerConductance
   REAL(dp) :: defaultTimeOfMaximumTemperature, defaultInstrumentHeight
   REAL(dp) :: bareSoilRoughness
   REAL(dp), ALLOCATABLE :: nodeDepth(:), thermCondPar1(:), thermCondPar2(:)
   REAL(dp), ALLOCATABLE :: thermCondPar3(:), thermCondPar4(:)
   REAL(dp) :: pom, soilRoughnessHeight, nu
   CHARACTER(:), allocatable :: boundarLayerConductanceSource, netRadiationSource
   REAL(dp) :: MissingValue
   integer, parameter :: MAX_LEN = 200
   CHARACTER(LEN=MAX_LEN), ALLOCATABLE :: soilConstituentNames(:)
   REAL(dp) :: netRadiation
   REAL(dp), ALLOCATABLE :: aveSoilWater(:), bulkDensity(:)
   REAL(dp) :: internalTimeStep
   REAL(dp), ALLOCATABLE :: thermalConductance(:), thickness(:)
   LOGICAL :: doInitialisationStuff
   REAL(dp) :: maxTempYesterday, timeOfDaySecs
   INTEGER :: numNodes, numLayers
   REAL(dp), ALLOCATABLE :: soilWater(:), clay(:), soilTemp(:), silt(:)
   REAL(dp) :: instrumentHeight
   REAL(dp), ALLOCATABLE :: sand(:), volSpecHeatSoil(:), heatStorage(:)
   REAL(dp), ALLOCATABLE :: minSoilTemp(:), maxSoilTemp(:), newTemperature(:)
   REAL(dp) :: airTemperature, instrumHeight, canopyHeight, awc
   REAL(dp), ALLOCATABLE :: thermalConductivity(:), morningSoilTemp(:), aveSoilTemp(:)
   REAL(dp), ALLOCATABLE :: carbon(:), rocks(:), InitialValues(:)
   REAL(dp) :: result(11, n_days), albedo, nbLayers, soilDepth, meanBd
   integer :: j
   LOGICAL::file_exists
   character(len=200) :: weather_folder, listfile, command, soildata, soilmetadata, weather_metadata, outputfolder
   integer :: iunit, iostat, file_count, iunit_wth, last_slash, isoil
   character(len=4) :: soil_types(4)
   character(len=4) :: soil
   character(len=12) :: date_str

   soil_types = (/'SICL', 'SAND', 'SILO', 'SALO'/) !  'SICL', 'SAND', 'SILO', 'SALO'


   weather_folder = '/mnt/d/Docs/AMEI_Workshop/apsimcampbellf90/Data/WeatherData'
   soildata  = "/mnt/d/Docs/AMEI_Workshop/apsimcampbellf90/Data/SoilData.txt"
   soilmetadata = "/mnt/d/Docs/AMEI_Workshop/apsimcampbellf90/Data/SoilMetadata.txt"
   weather_metadata = "/mnt/d/Docs/AMEI_Workshop/apsimcampbellf90/Data/WeatherMetadata.txt"
   outputfolder = '/mnt/d/Docs/AMEI_Workshop/apsimcampbellf90/Output_Fortran_Apsim'

   listfile = 'wth_list.txt'
   ! Create command to list .WTH files with full paths
   command = 'find "' // trim(weather_folder) // '" -name "*.WTH" > ' // trim(listfile)
   call execute_command_line(trim(command))
   open(newunit=iunit, file=listfile, status='old', action='read')
   file_count = 0

   unit = 10
   do
      read(iunit, '(A)', iostat=iostat) filepath
      if (iostat /= 0) exit

      filepath = adjustl(trim(filepath))
      call remove_newline(filepath)

      inquire(file=trim(filepath), exist=file_exists)
      if (.not. file_exists) then
         print *, 'ERREUR: Fichier non trouvé -> ', trim(filepath)
         cycle
      end if

      file_count = file_count + 1
      open(newunit=iunit_wth, file=trim(filepath), status='old', action='read', iostat=iostat)
      if (iostat /= 0) then
         print *, 'ERREUR: Impossible d ouvrir le fichier, iostat = ', iostat
         cycle
      end if

      last_slash = index(filepath, '/', back=.true.)
      if (last_slash > 0) then
         filename = filepath(last_slash+1:)
      else
         filename = filepath
      end if
      pos1 = INDEX(filepath, 'AW')
      pos2 = INDEX(filepath(pos1+2:), '.WTH') + pos1 + 1
      aw_value = filepath(pos1+2 : pos2-1)
      read(aw_value, *) awc
      lai_char = filename(6:6)
      read(lai_char, *) lai
      target_id = filename(1:4)
      read(iunit_wth, '(A)') line

      do i = 1, n_days
         read(iunit_wth, *, iostat=ios) date(i), t2m(i), tmin(i), tmax(i), rain(i), srad(i), &
            dayld(i), sunup(i), sundn(i), eoad(i), esp(i), es(i), le(i), g(i), snow(i)
         if (ios /= 0) exit
      end do
      close(iunit_wth)

      unit = 22
      do isoil = 1, size(soil_types)
         n = 0
         soil = soil_types(isoil)
         open(unit=unit, file=soildata, status="old", action="read")
         read(unit, '(A)') line
         read(unit, '(A)') line
         read(unit, '(A)') line
         do
            read(unit, *, iostat=ios) soil_idi, soil_namei, slidi, sllti, sllbi, thicki, &
               slbdmi, sloci, slsati, slduli, sllli, slclyi, slsili, slsndi, svsei
            if (ios /= 0) exit
            if (soil_idi == soil) then  ! <<==== Filter here
               n = n + 1
               soil_id(n) = soil_idi
               soil_name(n) = soil_namei
               slid(n) = slidi
               sllt(n) = sllti
               sllb(n) = sllbi
               thick(n) = thicki
               slbdm(n) = slbdmi
               sloc(n) = sloci
               slsat(n) = slsati
               sldul(n) = slduli
               slll(n) = sllli
               slcly(n) = slclyi
               slsil(n) = slsili
               slsnd(n) = slsndi
               svse(n) = svsei
            end if
         end do
         close(unit)

         open(unit=unit, file=soilmetadata, status='old', action='read')
         read(unit, '(A)') line
         read(unit, '(A)') line
         read(unit, '(A)') line
         do
            read(unit, *, iostat=ios) soil_idi, albedoi, nbLayersi, soilDepthi, meanBdi
            if (ios /= 0) exit
            if (soil_idi == soil) then
               albedo = albedoi
               nbLayers = nbLayersi
               soilDepth = soilDepthi
               meanBd = meanBdi
            end if
         end do
         close(unit)

         unit = 99
         open(unit=unit, file=weather_metadata, status='old', action='read')
         ! Skip the two header lines
         read(unit, '(A)') line
         read(unit, '(A)') line
         read(unit, '(A)') line
         do
            read(unit, *, iostat=ios) ws_id, site_namei, countryi, xlati, xlongi, tampi, tavi
            if (ios /= 0) exit
            if (trim(ws_id) == trim(target_id)) then
               xlat = xlati
               xlong = xlongi
               tamp = tampi
               tav = tavi
               site_name = site_namei
               country = countryi
            end if
         end do
         close(99)


         ps =  2.63
         airNode = 0
         surfaceNode = 1
         weather_MinT = tmin(1)
         weather_MaxT = tmax(1)
         weather_MeanT = t2m(1)
         weather_Tav = tav
         weather_Amp = tamp
         weather_AirPressure = 1010
         weather_Wind = 3.0
         weather_Latitude = xlat
         weather_Radn = srad(1)
         clock_Today_DayOfYear = mod(date(1), 1000)
         microClimate_CanopyHeight = 0.0
         waterBalance_Eos = esp(1)
         waterBalance_Eo =  eoad(1)
         waterBalance_Es = es(1)
         waterBalance_Salb = albedo
         DepthToConstantTemperature = 10000.0_8
         instrumentHeight = 1.2_8
         boundarLayerConductanceSource = 'calc'
         boundaryLayerConductance = 0.0_8
         timestep = 86400.0_8
         latentHeatOfVapourisation = 2465000.0_8
         stefanBoltzmannConstant = 5.67e-8_8
         topsoilNode = 2
         numPhantomNodes = 5
         constantBoundaryLayerConductance  = 20.0_8
         numIterationsForBoundaryLayerConductance = 1
         defaultTimeOfMaximumTemperature = 14.0_8
         defaultInstrumentHeight = 1.2_8
         bareSoilRoughness = 57.0_8
         boundaryLayerConductance = 0.0_8
         pom = 1.3_8
         nu = 0.6_8
         netRadiationSource = "calc"
         MissingValue = 999999
         soilRoughnessHeight = 0.0
         ALLOCATE(soilConstituentNames(8))
         soilConstituentNames(1) = "Rocks"
         soilConstituentNames(2) = "OrganicMatter"
         soilConstituentNames(3) = "Sand"
         soilConstituentNames(4) = "Silt"
         soilConstituentNames(5) = "Clay"
         soilConstituentNames(6) = "Water"
         soilConstituentNames(7) = "Ice"
         soilConstituentNames(8) = "Air"
         ALLOCATE(physical_Thickness(20), physical_BD(20), physical_Rocks(20))
         ALLOCATE(physical_ParticleSizeSand(20), physical_ParticleSizeSilt(20))
         ALLOCATE(physical_ParticleSizeClay(20), organic_Carbon(20), waterBalance_SW(20))
         physical_BD = slbdm(1:10)
         physical_Thickness = thick(1:10) * 10_8
         physical_Rocks = 0.0_8
         physical_ParticleSizeSand = slsnd(1:10)
         physical_ParticleSizeSilt = slsil(1:10)
         physical_ParticleSizeClay = slcly(1:10)
         organic_Carbon = sloc(1:10)
         waterBalance_SW = slll(1:10) + awc * (sldul(1:10) - slll(1:10))
         instrumHeight = 0.0_8

         CALL init_soiltemp(weather_MinT, weather_MaxT, weather_MeanT, weather_Tav, &
            weather_Amp, weather_AirPressure, weather_Wind, weather_Latitude, &
            weather_Radn, clock_Today_DayOfYear, microClimate_CanopyHeight, &
            physical_Thickness, physical_BD, ps, physical_Rocks, &
            physical_ParticleSizeSand, physical_ParticleSizeSilt, &
            physical_ParticleSizeClay, organic_Carbon, waterBalance_SW, &
            waterBalance_Eos, waterBalance_Eo, waterBalance_Es, waterBalance_Salb, &
            pInitialValues, DepthToConstantTemperature, timestep, &
            latentHeatOfVapourisation, stefanBoltzmannConstant, airNode, &
            surfaceNode, topsoilNode, numPhantomNodes, &
            constantBoundaryLayerConductance, numIterationsForBoundaryLayerConductance, &
            defaultTimeOfMaximumTemperature, defaultInstrumentHeight, bareSoilRoughness, &
            nodeDepth, thermCondPar1, thermCondPar2, thermCondPar3, thermCondPar4, &
            pom, soilRoughnessHeight, nu, boundarLayerConductanceSource, &
            netRadiationSource, MissingValue, soilConstituentNames, InitialValues, &
            doInitialisationStuff, internalTimeStep, timeOfDaySecs, numNodes, &
            numLayers, volSpecHeatSoil, soilTemp, morningSoilTemp, heatStorage, &
            thermalConductance, thermalConductivity, boundaryLayerConductance, &
            newTemperature, airTemperature, maxTempYesterday, minTempYesterday, &
            soilWater, minSoilTemp, maxSoilTemp, aveSoilTemp, aveSoilWater, &
            thickness, bulkDensity, rocks, carbon, sand, silt, clay, &
            instrumentHeight, netRadiation, canopyHeight, instrumHeight)
         !write(*,*) 'Init new temperature=', newTemperature
         do i = 1, n_days
            weather_MinT = tmin(i)
            weather_MaxT = tmax(i)
            weather_MeanT = t2m(i)
            weather_Radn = srad(i)
            clock_Today_DayOfYear = mod(date(i), 1000)
            waterBalance_Eos = esp(i)
            waterBalance_Eo =  eoad(i)
            waterBalance_Es = es(i)
            call model_soiltemp(netRadiation, &
               aveSoilWater, &
               bulkDensity, &
               waterBalance_Eo, &
               thermCondPar1, &
               topsoilNode, &
               surfaceNode, &
               internalTimeStep, &
               thermalConductance, &
               thickness, &
               numPhantomNodes, &
               soilConstituentNames, &
               doInitialisationStuff, &
               maxTempYesterday, &
               waterBalance_Salb, &
               physical_Thickness, &
               MissingValue, &
               timeOfDaySecs, &
               numNodes, &
               timestep, &
               organic_Carbon, &
               waterBalance_Es, &
               weather_Wind, &
               soilWater, &
               soilRoughnessHeight, &
               physical_ParticleSizeSand, &
               numIterationsForBoundaryLayerConductance, &
               clay, &
               weather_AirPressure, &
               soilTemp, &
               clock_Today_DayOfYear, &
               silt, &
               defaultTimeOfMaximumTemperature, &
               pom, &
               DepthToConstantTemperature, &
               microClimate_CanopyHeight, &
               constantBoundaryLayerConductance, &
               waterBalance_Eos, &
               instrumentHeight, &
               thermCondPar4, &
               waterBalance_SW, &
               weather_Amp, &
               nodeDepth, &
               nu, &
               sand, &
               pInitialValues, &
               weather_MinT, &
               ps, &
               netRadiationSource, &
               weather_Radn, &
               airNode, &
               numLayers, &
               volSpecHeatSoil, &
               instrumHeight, &
               canopyHeight, &
               heatStorage, &
               minSoilTemp, &
               bareSoilRoughness, &
               thermCondPar2, &
               defaultInstrumentHeight, &
               maxSoilTemp, &
               physical_BD, &
               latentHeatOfVapourisation, &
               weather_Latitude, &
               physical_Rocks, &
               stefanBoltzmannConstant, &
               weather_Tav, &
               newTemperature, &
               airTemperature, &
               weather_MaxT, &
               boundarLayerConductanceSource, &
               thermalConductivity, &
               minTempYesterday, &
               carbon, &
               weather_MeanT, &
               rocks, &
               InitialValues, &
               thermCondPar3, &
               physical_ParticleSizeSilt, &
               boundaryLayerConductance, &
               physical_ParticleSizeClay, &
               aveSoilTemp, &
               morningSoilTemp)

            result(:, i) = aveSoilTemp(2:12)  ! Store results for each day
         end do

         DEALLOCATE(physical_Thickness, physical_BD, physical_Rocks)
         DEALLOCATE(physical_ParticleSizeSand, physical_ParticleSizeSilt, physical_ParticleSizeClay)
         DEALLOCATE(organic_Carbon, waterBalance_SW)
         write(filename, '(A,"/SoilTemperature_FO_APC_",A,"_",A,"_L",I0,"_AW",F4.2,".txt")') &
            trim(outputfolder), trim(target_id), trim(soil), lai, awc

         filename = adjustl(filename)
         unit = 20
         open(unit=unit, file=trim(filename), status='replace')
         write(unit, '(A)') 'Date           SLLT    SLLB     TSLD'
         do i = 1, n_days
            year = date(i) / 1000
            doy = mod(date(i), 1000)
            call doy_to_month_day(year, doy, month, day)
            write(date_str, '(I4, "-", I2.2, "-", I2.2)') year, month, day
            write(20, '(A, I8, I8, F15.6)') trim(adjustl(date_str))//" ", 0, 0, result(1, i)
            do j = 1, 10
               write(20, '(A, I8, I8, F15.6)') trim(adjustl(date_str))//" ", int(sllt(j)), int(sllb(j)), result(j+1, i)
            end do
         end do
         close(20)

         if (ALLOCATED(soilConstituentNames)) DEALLOCATE(soilConstituentNames)
         if (ALLOCATED(pInitialValues)) DEALLOCATE(pInitialValues)
         if (ALLOCATED(nodeDepth)) DEALLOCATE(nodeDepth)
         if (ALLOCATED(thermCondPar2)) DEALLOCATE(thermCondPar1)
         if (ALLOCATED(thermCondPar2)) DEALLOCATE(thermCondPar2)
         if (ALLOCATED(thermCondPar3)) DEALLOCATE(thermCondPar3)
         if (ALLOCATED(thermCondPar4)) DEALLOCATE(thermCondPar4)
         if (ALLOCATED(volSpecHeatSoil)) DEALLOCATE(volSpecHeatSoil)
         if (ALLOCATED(soilTemp)) DEALLOCATE(soilTemp)
         if (ALLOCATED(morningSoilTemp)) DEALLOCATE(morningSoilTemp)
         if (ALLOCATED(heatStorage)) DEALLOCATE(heatStorage)
         if (ALLOCATED(thermalConductance)) DEALLOCATE(thermalConductance)
         if (ALLOCATED(thermalConductivity)) DEALLOCATE(thermalConductivity)
         if (ALLOCATED(newTemperature)) DEALLOCATE(newTemperature)
         if (ALLOCATED(soilWater)) DEALLOCATE(soilWater)
         if (ALLOCATED(minSoilTemp)) DEALLOCATE(minSoilTemp)
         if (ALLOCATED(maxSoilTemp)) DEALLOCATE(maxSoilTemp)
         if (ALLOCATED(aveSoilTemp)) DEALLOCATE(aveSoilTemp)
         if (ALLOCATED(aveSoilWater)) DEALLOCATE(aveSoilWater)
         if (ALLOCATED(thickness)) DEALLOCATE(thickness)
         if (ALLOCATED(bulkDensity)) DEALLOCATE(bulkDensity)
         if (ALLOCATED(rocks)) DEALLOCATE(rocks)
         if (ALLOCATED(carbon)) DEALLOCATE(carbon)
         if (ALLOCATED(sand)) DEALLOCATE(sand)
         if (ALLOCATED(silt)) DEALLOCATE(silt)
         if (ALLOCATED(clay)) DEALLOCATE(clay)
         if (ALLOCATED(InitialValues)) DEALLOCATE(InitialValues)
      end do
   end do

contains
   subroutine remove_newline(string)
      character(len=*), intent(inout) :: string
      integer :: ii, len_str

      len_str = len_trim(string)
      do ii = 1, len_str
         if (iachar(string(ii:ii)) < 32 .and. iachar(string(ii:ii)) /= 9) then  ! 9 = tabulation
            string(ii:ii) = ' '
         end if
      end do
      string = adjustl(trim(string))
   end subroutine remove_newline

   subroutine doy_to_month_day(year, doy, month, day)
      integer, intent(in) :: year, doy
      integer, intent(out) :: month, day
      integer :: months(12), k, cumulative_days
      logical :: leap_year

      leap_year = (mod(year, 4) == 0 .and. .not. (mod(year, 100) == 0 .and. mod(year, 400) /= 0))

      months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      if (leap_year) months(2) = 29

      cumulative_days = 0
      do k = 1, 12
         cumulative_days = cumulative_days + months(k)
         if (doy <= cumulative_days) then
            month = k
            day = doy - (cumulative_days - months(k))
            return
         end if
      end do

      month = 1
      day = 1
   end subroutine doy_to_month_day
END PROGRAM main
