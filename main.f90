PROGRAM main
    USE Soiltempmod, ONLY: init_soiltemp, model_soiltemp
    IMPLICIT NONE

    integer, parameter :: n_days = 10958    ! Adjust to your file length
    real :: tmin(n_days), tmax(n_days), t2m(n_days), rain(n_days), srad(n_days), &
            dayld(n_days), sunup(n_days), sundn(n_days), eoad(n_days), &
            esp(n_days), le(n_days), g(n_days), snow(n_days), es(n_days)
    integer :: i, ios, date(n_days), doy
    character(len=256) :: line, filename
    integer :: unit

    integer, parameter ::  max_layers = 100
    character(len=10)  :: soil_idi, soil_id(max_layers)
    character(len=20)  :: soil_namei, soil_name(max_layers)
    integer            :: slidi, slid(max_layers)
    real               :: sllti, sllbi, thicki, slbdmi, sloci, slsati, slduli, &
                         sllli, slclyi, slsili, slsndi, svsei
    real               :: sllt(max_layers), sllb(max_layers), thick(max_layers)
    real               :: slbdm(max_layers), sloc(max_layers), slsat(max_layers)
    real               :: sldul(max_layers), slll(max_layers), slcly(max_layers), &
                           SLSIL(max_layers), SLSND(max_layers), svse(max_layers)
    integer ::  n, year, month, day

    character(len=10)  :: ws_id, target_id
    character(len=30)  :: site_name, country, site_namei, countryi
    real :: xlat, xlong, tamp, tav
    real :: xlati, xlongi, tampi, tavi
	integer :: lai
    
    character(len=250) :: filepath
    character(len=20)  :: aw_value, lai_char
    integer :: pos1, pos2


    ! Declare variables
    REAL :: weather_MinT, weather_MaxT, weather_MeanT
    REAL :: weather_Tav, weather_Amp, weather_AirPressure, weather_Wind
    REAL :: weather_Latitude, weather_Radn
    INTEGER :: clock_Today_DayOfYear
    REAL :: microClimate_CanopyHeight,  boundaryLayerConductance, mintempyesterday
    REAL, ALLOCATABLE :: physical_Thickness(:), physical_BD(:)
    REAL :: ps
    REAL, ALLOCATABLE :: physical_Rocks(:), physical_ParticleSizeSand(:)
    REAL, ALLOCATABLE :: physical_ParticleSizeSilt(:), physical_ParticleSizeClay(:)
    REAL, ALLOCATABLE :: organic_Carbon(:), waterBalance_SW(:)
    REAL :: waterBalance_Eos, waterBalance_Eo, waterBalance_Es, waterBalance_Salb
    REAL, ALLOCATABLE :: pInitialValues(:)
    REAL :: DepthToConstantTemperature, timestep, latentHeatOfVapourisation
    REAL :: stefanBoltzmannConstant
    INTEGER :: airNode, surfaceNode, topsoilNode, numPhantomNodes
    REAL :: constantBoundaryLayerConductance
    INTEGER :: numIterationsForBoundaryLayerConductance
    REAL :: defaultTimeOfMaximumTemperature, defaultInstrumentHeight
    REAL :: bareSoilRoughness
    REAL, ALLOCATABLE :: nodeDepth(:), thermCondPar1(:), thermCondPar2(:)
    REAL, ALLOCATABLE :: thermCondPar3(:), thermCondPar4(:)
    REAL :: pom, soilRoughnessHeight, nu
    CHARACTER(:), allocatable :: boundarLayerConductanceSource, netRadiationSource
    REAL :: MissingValue
    !Type DLstring_t
        !Character(LEN=:), allocatable :: aString
    !End Type
    !Type(DLstring_t), ALLOCATABLE :: soilConstituentNames(:)
    integer, parameter :: MAX_LEN = 200
    CHARACTER(LEN=MAX_LEN), ALLOCATABLE :: soilConstituentNames(:)
    REAL :: netRadiation
    REAL, ALLOCATABLE :: aveSoilWater(:), bulkDensity(:)
    REAL :: internalTimeStep
    REAL, ALLOCATABLE :: thermalConductance(:), thickness(:)
    LOGICAL :: doInitialisationStuff
    REAL :: maxTempYesterday, timeOfDaySecs
    INTEGER :: numNodes, numLayers
    REAL, ALLOCATABLE :: soilWater(:), clay(:), soilTemp(:), silt(:)
    REAL :: instrumentHeight
    REAL, ALLOCATABLE :: sand(:), volSpecHeatSoil(:), heatStorage(:)
    REAL, ALLOCATABLE :: minSoilTemp(:), maxSoilTemp(:), newTemperature(:)
    REAL :: airTemperature, instrumHeight, canopyHeight, awc
    REAL, ALLOCATABLE :: thermalConductivity(:), morningSoilTemp(:), aveSoilTemp(:)
    REAL, ALLOCATABLE :: carbon(:), rocks(:), InitialValues(:)
    real :: result(11, n_days)
    integer :: j
	LOGICAL::file_exists
    
    character(len=200) :: weather_folder, listfile, command
	integer :: iunit, iostat, file_count, iunit_wth, last_slash, isoil
	
    character(len=4) :: soil_types(4) 
    character(len=4) :: soil
	
    soil_types = (/ 'SAND','SICL', 'SILO', 'SALO' /) ! , 'SICL', 'SILO', 'SALO' 
    print *, size(soil_types)

    unit = 10
	
	

    weather_folder = '/mnt/d/Docs/AMEI_Workshop/apsimcampbellf90/Data/WeatherData'
    listfile = 'wth_list.txt'

    
    ! Create command to list .WTH files with full paths
    command = 'find "' // trim(weather_folder) // '" -name "*.WTH" > ' // trim(listfile)
    
    ! Execute the command
    call execute_command_line(trim(command))
    
    ! Read the list file
    open(newunit=iunit, file=listfile, status='old', action='read')
    file_count = 0
    
    do
	
	    read(iunit, '(A)', iostat=iostat) filepath
        if (iostat /= 0) exit

        filepath = adjustl(trim(filepath))
		call remove_newline(filepath)
		print *, 'my weather file', trim(filepath)
		
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
		print *, 'awc', awc
        
        lai_char = filename(6:6)
		print *, 'lai_char', lai_char
        read(lai_char, *) lai
		print *, 'lai', lai
        
        ! Get "Country" in the filename
        target_id = filename(1:4)
		print *, 'target_id', target_id
        
        ! Skip header
        read(iunit_wth, '(A)') line

        do i = 1, n_days
            read(iunit_wth, *, iostat=ios) date(i), t2m(i), tmin(i), tmax(i), rain(i), srad(i), &
                     dayld(i), sunup(i), sundn(i), eoad(i), esp(i), es(i), le(i), g(i), snow(i)
            if (ios /= 0) exit
        end do

        close(iunit_wth)

        unit = 22
        open(unit=unit, file="/mnt/d/Docs/AMEI_Workshop/apsimcampbellf90/Data/SoilData.txt", status="old", action="read")

        ! Skip headers
        read(unit, '(A)') line
        read(unit, '(A)') line
        read(unit, '(A)') line
		
		
		do isoil = 1, size(soil_types)
			n = 0
			soil = soil_types(isoil) !soil_types(i) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

			! Now your arrays *_arr(:n) contain only layers with Soil ID = 'SICL'
			print *, 'Number of SAND layers:', n
			print *, 'Bulk densities:'
			print *, slbdm(1:n)

			print *, "eoad: ", eoad(1:n_days)


			!target_id = 'CAQC'    ! Set the weather station ID you want
			unit = 99

			open(unit=unit, file="/mnt/d/Docs/AMEI_Workshop/apsimcampbellf90/Data/WeatherMetadata.txt", status='old', action='read')

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

			print *, 'Site name: ', trim(site_name)
			print *, 'Country:   ', trim(country)
			print *, 'Latitude:  ', xlat
			print *, 'Longitude: ', xlong
			print *, 'Amplitude: ', tamp
			print *, 'Tav:       ', tav

			! Initialize variables (example values)
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
			ps =  2.63
			waterBalance_Eos = esp(1)
			print *, "waterBalance_Eos: ", waterBalance_Eos
			waterBalance_Eo =  eoad(1)
			print *, "waterBalance_Eo: ", waterBalance_Eo
			waterBalance_Es = es(1) 
			print *, "waterBalance_Es: ", waterBalance_Es

			waterBalance_Salb = 0.15
			!awc = 0.0


			print *, "clock_Today_DayOfYear: ", clock_Today_DayOfYear

			DepthToConstantTemperature = 10000.0
			instrumentHeight = 1.2
			boundarLayerConductanceSource = 'calc'
			print *, "boundarLayerConductanceSource: ", boundarLayerConductanceSource, LEN(boundarLayerConductanceSource)
			boundaryLayerConductance = 0.0
			timestep = 86400.0
			latentHeatOfVapourisation = 2.465e6
			stefanBoltzmannConstant = 5.67e-8
			airNode = 0
			surfaceNode = 1
			topsoilNode = 2
			numPhantomNodes = 5
			constantBoundaryLayerConductance  = 20
			numIterationsForBoundaryLayerConductance = 1
			defaultTimeOfMaximumTemperature = 14.0 
			defaultInstrumentHeight = 1.2
			bareSoilRoughness = 57
			boundaryLayerConductance = 0.0
			pom = 1.3
			nu = 0.6
			netRadiationSource = "calc"
			MissingValue = 999999


			!ALLOCATE(soilConstituentNames(8))
			ALLOCATE(soilConstituentNames(8))

			! Assign strings of different lengths
			! Assign strings of different lengths
			soilConstituentNames(1) = "Rocks"
			soilConstituentNames(2) = "OrganicMatter"
			soilConstituentNames(3) = "Sand"
			soilConstituentNames(4) = "Silt"
			soilConstituentNames(5) = "Clay"
			soilConstituentNames(6) = "Water"
			soilConstituentNames(7) = "Ice"
			soilConstituentNames(8) = "Air"

			! Print the assigned value
			PRINT *, "Element 8:", soilConstituentNames(8)

			! Allocate arrays
			ALLOCATE(physical_Thickness(20), physical_BD(20), physical_Rocks(20))
			ALLOCATE(physical_ParticleSizeSand(20), physical_ParticleSizeSilt(20))
			ALLOCATE(physical_ParticleSizeClay(20), organic_Carbon(20), waterBalance_SW(20))


			PRINT *, "Element 8:", LEN(soilConstituentNames(8))

			! Assign example values to arrays
			physical_Thickness = thick(1:10) * 10
			physical_BD = slbdm(1:10)
			physical_Rocks = 0
			physical_ParticleSizeSand = slsnd(1:10)
			physical_ParticleSizeSilt = slsil(1:10)
			physical_ParticleSizeClay = slcly(1:10)
			organic_Carbon = sloc(1:10)
			waterBalance_SW = slll(1:10) + awc * (sldul(1:10) - slll(1:10))
			instrumHeight = 0
			! print waterBalance_SW
			print *, "slll: ", slll(1:10)
			print *, "sldul: ", sldul(1:10)
			PRINT *, "waterBalance_SW: ", waterBalance_SW

			PRINT *, "Element 8:", soilConstituentNames(7)	

			! Call the subroutine
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

			! Output results (example)
			PRINT *, "Initialisation complete!"
			PRINT *, "Net Radiation:", netRadiation
			PRINT *, "Soil Temperature:", soilTemp


			do i = 1, n_days
				weather_MinT = tmin(i)
				weather_MaxT = tmax(i)
				weather_MeanT = t2m(i)
				weather_Radn = srad(i)
				clock_Today_DayOfYear = mod(date(i), 1000)
				microClimate_CanopyHeight = 0.0
				ps =  2.63
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

					! Output results (example)
				result(:, i) = aveSoilTemp(2:12)  ! Store results for each day
				if ( i == 1 ) then
					print *, "Day:", date(i), "Soil Temperature:", aveSoilTemp(2:13)
				end if

				! Deallocate arrays

			end do

			DEALLOCATE(physical_Thickness, physical_BD, physical_Rocks)
			DEALLOCATE(physical_ParticleSizeSand, physical_ParticleSizeSilt, physical_ParticleSizeClay)
			DEALLOCATE(organic_Carbon, waterBalance_SW)

			write(filename, '(A,"/SoilTemperature_FO_APC_",A,"_",A,"_L",I0,"_AW",F4.2,".txt")') &
			 "/mnt/d/Docs/AMEI_Workshop/apsimcampbellf90/Output_Fortran_Apsim", trim(target_id), trim(soil), lai, awc

			filename = adjustl(filename)
			unit = 20
			open(unit=unit, file=trim(filename), status='replace')
			write(unit, '(A10, A8, A9, A13)') 'Date', 'SLLT', 'SLLB', 'TSLD'
			
			do i = 1, n_days
				year = date(i) / 1000
				doy = mod(date(i), 1000)
				call doy_to_month_day(year, doy, month, day)
				write(20, '(I4, "-", I2.2, "-", I2.2, 1X, F8.0, 1X, F8.0, F12.3)') year,month, day,0.0,0.0, result(1, i)
				do j = 1, 10
					write(20, '(I4, "-", I2.2, "-", I2.2, 1X, F8.0, 1X, F8.0, F12.3)') year,month, day,sllt(j),sllb(j), result(j+1, i)
				end do
			end do

			close(20)
		
			! Désallocation de toutes les variables ALLOCATABLE
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
		close(iunit_wth)
    end do
	close(iunit)
	
	contains
    subroutine remove_newline(string)
        character(len=*), intent(inout) :: string
        integer :: i, len_str
        
        len_str = len_trim(string)
        do i = 1, len_str
            if (iachar(string(i:i)) < 32 .and. iachar(string(i:i)) /= 9) then  ! 9 = tabulation
                string(i:i) = ' '
            end if
        end do
        string = adjustl(trim(string))
    end subroutine remove_newline
	
    subroutine doy_to_month_day(year, doy, month, day)
        integer, intent(in) :: year, doy
        integer, intent(out) :: month, day
        integer :: months(12), i, cumulative_days
        logical :: leap_year
        
        leap_year = (mod(year, 4) == 0 .and. .not. (mod(year, 100) == 0 .and. mod(year, 400) /= 0))
        
        months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        if (leap_year) months(2) = 29
        
        cumulative_days = 0
        do i = 1, 12
            cumulative_days = cumulative_days + months(i)
            if (doy <= cumulative_days) then
                month = i
                day = doy - (cumulative_days - months(i))
                return
            end if
        end do
        
        month = 1
        day = 1
    end subroutine doy_to_month_day	
	


END PROGRAM main
