!>
!! @mainpage Los Alamos Sea Ice (CICE) NUOPC Cap
!! @author Fei Liu (fei.liu@gmail.com)
!! @author Rocky Dunlap (rocky.dunlap@noaa.gov)
!! @date 5/10/13 Original documentation
!! @date 10/6/16 Moved to doxygen
!!
!! @tableofcontents
!!
!! @section Overview Overview
!!
!! **This CICE cap has been tested with versions 4.0 and 5.0.2 of CICE.**
!!
!! This document describes the CICE "cap", which is a small software layer that is
!! required when the [Los Alamos sea ice model] (http://oceans11.lanl.gov/trac/CICE)
!! is used in [National Unified Operation Prediction Capability]
!! (http://www.earthsystemcog.org/projects/nuopc) (NUOPC) coupled systems.
!! The NUOPC Layer is a software layer built on top of the [Earth System Modeling
!! Framework] (https://www.earthsystemcog.org/projects/esmf) (ESMF).
!! ESMF is a high-performance modeling framework that provides
!! data structures, interfaces, and operations suited for building coupled models
!! from a set of components. NUOPC refines the capabilities of ESMF by providing
!! a more precise definition of what it means for a model to be a component and
!! how components should interact and share data in a coupled system. The NUOPC
!! Layer software is designed to work with typical high-performance models in the
!! Earth sciences domain, most of which are written in Fortran and are based on a
!! distributed memory model of parallelism (MPI).
!! A NUOPC "cap" is a Fortran module that serves as the interface to a model
!! when it's used in a NUOPC-based coupled system.
!! The term "cap" is used because it is a small software layer that sits on top
!! of model code, making calls into it and exposing model data structures in a
!! standard way. For more information about creating NUOPC caps in general, please
!! see the [Building a NUOPC Model]
!! (http://www.earthsystemmodeling.org/esmf_releases/non_public/ESMF_7_0_0/NUOPC_howtodoc/)
!! how-to document.
!!
!!
!! The CICE cap package contains a single Fortran source file and two makefiles. The Fortran
!! source file (cice_cap.F90) implements a standard Fortran module that uses ESMF
!! and NUOPC methods and labels to create a NUOPC cap for CICE. It also uses
!! variables and subroutines from CICE modules to invoke CICE subroutines and
!! exchange data. In the simplest terms, the CICE cap should be thought of as a thin
!! translation layer between the CICE Fortran code and the NUOPC infrastrucutre.
!! The source file can be built into a linkable library and
!! Fortran module file (.mod) for inclusion in other Fortran programs.
!!
!! @image html docimages/CAP_Anatomy_small.jpg "Architecture diagram showing how the CICE cap is an interface layer between the model code and the NUOPC coupling infrastructure"
!!
!! @subsection CapSubroutines Cap Subroutines
!!
!! The CICE cap Fortran module contains a set of subroutines that are required
!! by NUOPC.  These subroutines are called by the NUOPC infrastructure according
!! to a predefined calling sequence.  Some subroutines are called during
!! initialization of the coupled system, some during the run of the coupled
!! system, and some during finalization of the coupled system.  The initialization
!! sequence is the most complex and is governed by the NUOPC technical rules.
!! Details about the initialization sequence can be found in the [NUOPC Reference Manual]
!! (http://www.earthsystemmodeling.org/esmf_releases/non_public/ESMF_7_0_0/NUOPC_refdoc/node3.html#SECTION00034000000000000000).
!!
!! A particularly important part of the NUOPC intialization sequence is to establish
!! field connections between models.  Simply put, a field connection is established
!! when a field output by one model can be consumed by another.  As an example, the
!! CICE model is able to accept a precipitation rate when coupled to an atmosphere
!! model.  In this case a field connection will be established between the precipitation
!! rate exported from the atmosphere and the precipitation rate imported into the
!! CICE model.  Because models may uses different variable names for physical
!! quantities, NUOPC relies on a set of standard names and a built-in, extensible
!! standard name dictionary to match fields between models.  More information about
!! the use of standard names can be found in the [NUOPC Reference Manual]
!! (http://www.earthsystemmodeling.org/esmf_releases/non_public/ESMF_7_0_0/NUOPC_refdoc/node3.html#SECTION00032000000000000000).
!!
!! Two key initialization phases that appear in every NUOPC cap, including this CICE
!! cap are the field "advertise" and field "realize" phases.  *Advertise* is a special
!! NUOPC term that refers to a model participating in a coupled system
!! providing a list of standard names of required import fields and available export
!! fields.  In other words, each model will advertise to the other models which physical fields
!! it needs and which fields it can provide when coupled. NUOPC compares all of the advertised
!! standard names and creates a set of unidirectional links, each from one export field
!! in a model to one import field in another model.  When these connections have been established,
!! all models in the coupled system need to provide a description of their geographic
!! grid (e.g., lat-lon, tri-polar, cubed sphere, etc.) and allocate their connected
!! fields on that grid.  In NUOPC terms, this is refered to as *realizing* a set of
!! fields.  NUOPC relies on ESMF data types for this, such as the [ESMF_Grid]
!! (http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node5.html#SECTION05080000000000000000)
!! type, which describes logically rectangular grids and the [ESMF_Field]
!! (http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node5.html#SECTION05030000000000000000)
!! type, which wraps a models data arrays and provides basic metadata. Because ESMF supports
!! interpolation between different grids (sometimes called "regridding" or "grid remapping"),
!! it is not necessary that models share a grid.  As you will see below
!! the *advertise* and *realize* phases each have a subroutine in the CICE cap.
!!
!! The following table summarizes the NUOPC-required subroutines that appear in the
!! CICE cap.  The "Phase" column says whether the subroutine is called during the
!! initialization, run, or finalize part of the coupled system run.
!!
!!
!! Phase    | CICE Cap Subroutine                                                |  Description
!! ---------|--------------------------------------------------------------------|-------------------------------------------------------------
!! Init     | [InitializeP0] (@ref cice_cap_mod::initializep0)                   | Sets the Initialize Phase Definition (IPD) version to use
!! Init     | [InitializeAdvertise] (@ref cice_cap_mod::initializeadvertise)     | Advertises standard names of import and export fields
!! Init     | [InitializeRealize] (@ref cice_cap_mod::initializerealize)         | Creates an ESMF_Grid for the CICE grid as well as ESMF_Fields for import and export fields
!! Init     | [SetClock] (@ref cice_cap_mod::setclock)                           | Before the run, sets up the timestep interval
!! Run      | [ModelAdvance_Slow] (@ref cice_cap_mod::modeladvance_slow)         | Advances the model by a timestep by calling CICE_Run
!! Final    | [CICE_Model_Finalize] (@ref cice_cap_mod::cice_model_finalize)     | Cleans up by calling CICE_Finalize
!!
!! @section UnderlyingModelInterfaces Underlying Model Interfaces
!!
!! The following diagram shows a diagram of how the CICE cap code is structured.  Red boxes are
!! subroutines that are registered with NUOPC.  Light blue boxes are calls into CICE subroutines.
!! Yellow boxes are calls into the ESMF/NUOPC library.
!!
!! @image html docimages/CICE_CAP_Methods_small.png "A high level view of the code organization of the CICE cap"
!!
!! @subsection DomainCreation Domain Creation
!!
!! As outlined in section 4.2 of the [CICE user's guide]
!! (http://oceans11.lanl.gov/trac/CICE/attachment/wiki/WikiStart/cicedoc.pdf?format=raw),
!! the CICE model supports several kinds of grids: regular rectangular, regional,
!! displaced_pole, and tripolar.  The CICE cap currently only supports the
!! tripolar grid with a single southern pole and a bipole in northern latitudes.
!! The plots below show the latitude and longitude coordinate values (radians)
!! for a 0.5 degree (720x410 cells) tripolar grid.
!!
!!  <table border="0"><tr>
!!  <td>@image html docimages/cice_grid_ulat.png "Latitude coordinates of tripolar grid"</td>
!!  <td>@image html docimages/cice_grid_ulon.png "Longitude coordinates of tripolar grid"</td>
!!  </tr></table>
!!
!! No changes were made to the native CICE mechanisms for setting up the grid.
!! This procedure happens as part of the call to `CICE_Initialize()`.  After CICE sets up
!! its grid, the grid structure is translated into an `ESMF_Grid` object.  Setting
!! up the `ESMF_Grid` is handled as part of the [InitializeRealize]
!! (@ref cice_cap_mod::initializerealize) subroutine.  All of the details of the tripolar
!! grid must be provided to ESMF, including:
!!   - the global number of cells in the X and Y directions,
!!   - how the global grid is decomposed into blocks,
!!   - the connectivity along the southern and northern rows (to set up the northern bipole),
!!   - the periodic boundary condition in the X direction,
!!   - the center (called "T cell" in the CICE manual) and corner coordinates ("U cell") for all grid points,
!!   - the grid cell areas, and
!!   - the grid cell land/sea mask.
!!
!! CICE already has data structures to represent all of these grid features. However, NUOPC
!! infrastrucuture requires a standard representation of grids using ESMF types. Since the
!! tripolar grid is a logically rectangular grid (there is an underlying X-Y index space),
!! the [ESMF_Grid] (http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node5.html#SECTION05080000000000000000)
!! type is used.  Setting up the `ESMF_Grid` requires accessing some of the ice domain
!! variables inside CICE.  Refer to the "use" statements at the top of the cap (listed below)
!! to see which domain variables are accessed by the cap.
!!
!! @code
!!  use ice_blocks, only: nx_block, ny_block, nblocks_tot, block, get_block, &
!!                        get_block_parameter
!!  use ice_domain_size, only: max_blocks, nx_global, ny_global
!!  use ice_domain, only: nblocks, blocks_ice, distrb_info
!!  use ice_distribution, only: ice_distributiongetblockloc
!!  use ice_constants, only: Tffresh, rad_to_deg
!!  use ice_calendar,  only: dt
!!  use ice_flux
!!  use ice_grid, only: TLAT, TLON, ULAT, ULON, hm, tarea, ANGLET, ANGLE, &
!!                      dxt, dyt, t2ugrid_vector
!!  use ice_state
!!  use CICE_RunMod
!!  use CICE_InitMod
!!  use CICE_FinalMod
!! @endcode
!!
!! The ESMF_Grid is set up in several steps inside [InitializeRealize]
!! (@ref cice_cap_mod::initializerealize). The `deBlockList` variables is used
!! to store the min and max indices of each decomposition block. This is populated
!! using index values from the CICE subroutine `get_block_parameters()`. The
!! `petMap` array is used to store, for each block, the processor that is
!! assigned to that block.  This information is retrieved by calling the
!! CICE subroutine `ice_distributionGetBlockLoc()`.  The `petMap` is then used
!! to create an [ESMF_DELayout]
!! (http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_refdoc/node6.html#SECTION060110000000000000000),
!! which describes how decomposition elements (blocks in CICE terms) are assigned
!! to PETs (processors).
!!
!! The global CICE grid index space (`nx_global` x `ny_global`) is set up as an [ESMF_DistGrid]
!! (http://www.earthsystemmodeling.org/esmf_releases/last_built/ESMF_refdoc/node5.html#SECTION050120000000000000000)
!! by passing it the `deBlockList`, `delayout`, and a `connectionList` which defines
!! the periodic boundary condition in the X direction and bipole at the top. Finally,
!! the `ESMF_Grid` is created from the `ESMF_DistGrid`. The `ESMF_Grid` will use the
!! index space defined by the `ESMF_DistGrid` and geographic coordinates are assigned
!! to cell centers and corners.  The center coordinates are populating using the CICE
!! `TLON` and `TLAT` arrays and the corners using the `ULON` and `ULAT` arrays. Also
!! assigned are the cell areas (using the `tarea` array) and the grid mask (using the
!! `hm` array).  Details of how the CICE grid is set up internally in the model is
!! beyond the scope of this cap documentation, however the reader is refered to
!! section 4.2 of the [CICE user's guide] (http://oceans11.lanl.gov/trac/CICE/attachment/wiki/WikiStart/cicedoc.pdf?format=raw)
!! for more information.
!!
!!
!! @subsection Initialization Initialization
!!
!! The CICE cap calls into the native `CICE_Initialize()` subroutine in the
!! [InitializeAdvertise] (@ref cice_cap_mod::initializeadvertise) subroutine.
!! The only parameter passed is the MPI communicator.  The global MPI communicator
!! is managed by ESMF and is retrieved from the current `ESMF_VM`.
!!
!! @subsection Run Run
!!
!! The CICE cap calls into the native `CICE_Run()` subroutine in the
!! [ModelAdvance_Slow] (@ref cice_cap_mod::modeladvance_slow) subroutine. The internal
!! CICE timestepping loop has been disabled when using the NUOPC cap since the
!! main driver loop is provided by the NUOPC infrastructure at a level above
!! the CICE model.  Therefore, a call into `CICE_Run()` will only advance the
!! CICE model by a single time step.
!!
!! Prior to calling `CICE_Run()` the cap copies fields from the import
!! `ESMF_State` into the corresponding data arrays inside the model. The import state
!! contains a set of fields brought in through the NUOPC coupling infrastructure.
!! These native model arrays are defined in the file ice_flux.F90.
!! After copying in the import state fields, the call is made to `CICE_Run()`.  After
!! the run, model fields are copied into the export `ESMF_State` so that NUOPC can
!! transfer these fields to other models as required.
!!
!! Per field diagnostic output can be produced at the end of each run phase as
!! described in the [I/O section] (@ref IO).
!!
!! @subsubsection VectorRotations Vector Rotations
!!
!! Vector rotation is done for import and export 2D vector fields in the NUOPC cap.
!! In CICE cap, this happens inside of [ModelAdvance_Slow] (@ref cice_cap_mod::modeladvance_slow).
!! The vector fields are rotated from a regular lat-lon grid to the CICE grid before
!! `CICE_Run` and then rotated back to regular lat-lon grid after `CICE_Run`. The effect
!! of this is most obvious in the northern polar region where CICE operates on a
!! tripolar grid. Results on the T cell are also transformed to the U cell through
!! CICE internal method t2ugrid_vector.
!!
!! The following assumptions are made during vector rotation:
!!
!! - The components of the 2D vector fields line up with local lat-lon direction
!!   where north and east are the local positive directions. The
!!   components are defined at cell center for all the grids.
!!
!! - The rotation angles are positive when measured counter
!!   clockwise (CCW) from local lat-lon directions. This is the conventional defintion
!!   for the angular dimension in a polar coordinate system.
!!
!! As such, the imported vector field V must be rotated locally as the model component
!! local coordinate system is rotated CCW from well-defined lat-lon directions.
!! The CCW rotation matrix M is
!!
!!     cos \theta         sin \theta
!!     -sin \theta        cos \theta
!!
!!  Rotated vector V' = M V
!!
!! The transpose of this matrix corresponding to a CW coordinate system rotation is
!! applied to the exported 2D vector fields before they are exported.
!!
!! @subsection Finalization Finalization
!!
!! To finalize the model, the CICE cap simple calls into the native `CICE_Finalize()`
!! subroutine.
!!
!!
!! @section ModelFields Model Fields
!!
!! The following tables list the import and export fields currently set up in the CICE cap.
!!
!! @subsection ImportFields Import Fields
!!
!! Standard Name                     | Units      | Model Variable  | File         | Description                     | Notes
!! ----------------------------------|------------|-----------------|--------------|---------------------------------|--------------------------------------
!! air_density_height_lowest         | kg m-3     | rhoa            | ice_flux.F90 | air density                     | |
!! freezing_melting_potential        | W m-2      | frzmlt          | ice_flux.F90 | freezing/melting potential      | |
!! inst_height_lowest                | m          | zlvl            | ice_flux.F90 | height of lowest level          | |
!! inst_merid_wind_height_lowest     | m-2        | vatm            | ice_flux.F90 | wind v component                | [vector rotation applied] (@ref VectorRotations)
!! inst_pres_height_lowest           | Pa         | (none)          |              | pressure at lowest level        | used to calculate potT (potential temperature)
!! inst_spec_humid_height_lowest     | kg kg-1    | Qa              | ice_flux.F90 | specific humidity               | |
!! inst_temp_height_lowest           | K          | Tair            | ice_flux.F90 | near surface air temperature    | |
!! inst_zonal_wind_height_lowest     | m-2        | uatm            | ice_flux.F90 | wind u component                | [vector rotation applied] (@ref VectorRotations)
!! mean_down_lw_flx                  | W m-2      | flw             | ice_flux.F90 | downward longwave flux          | |
!! mean_down_sw_vis_dir_flx          | W m-2      | swvdr           | ice_flux.F90 | downward shortwave visible direct flux  | |
!! mean_down_sw_vis_dif_flx          | W m-2      | swvdf           | ice_flux.F90 | downward shortwave visible diffuse flux | |
!! mean_down_sw_ir_dir_flx           | W m-2      | swidr           | ice_flux.F90 | downward shortwave near infrared direct flux  | |
!! mean_down_sw_ir_dif_flx           | W m-2      | swidf           | ice_flux.F90 | downward shortwave near infrared diffuse flux | |
!! mean_fprec_rate                   | kg m-2 s-1 | fsnow           | ice_flux.F90 | snowfall rate                   | |
!! mean_prec_rate                    | kg m-2 s-1 | frain           | ice_flux.F90 | rainfall rate                   | |
!! (none)                            | W m-2      | fsw             | ice_flux.F90 | downward shortwave flux         | cap sets fsw as sum of shortwave components
!! ocn_current_merid                 | m-2        | vocn            | ice_flux.F90 | ocean current v component       | [vector rotation applied] (@ref VectorRotations) |
!! ocn_current_zonal                 | m-2        | uocn            | ice_flux.F90 | ocean current u component       | [vector rotation applied] (@ref VectorRotations) |
!! sea_surface_temperature           | C          | sst             | ice_flux.F90 | sea surface temperature         | converted from Kelvin to Celcius |
!! s_surf                            | ppt        | sss             | ice_flux.F90 | sea surface salinity            | |
!! sea_lev                           | m m-1      | ss_tltx, sstlty | ice_flux.F90 | sea surface slope in x & y      | sea_lev used to compute slope components, then [vector rotation applied] (@ref VectorRotations), then `t2ugrid_vectors()` called to move slope components to U grid
!!
!! These fields are advertised as imports but not currently used by the cap:
!! - inst_surface_height
!! - inst_temp_height2m
!! - inst_spec_humid_height2m
!! - mean_merid_moment_flx
!! - mean_zonal_moment_flx
!! - mixed_layer_depth
!! - sea_surface_slope_zonal
!! - sea_surface_slope_merid
!!
!! @subsection ExportField Export Fields
!!
!! Standard Name                     | Units      | Model Variable  | File          | Description                     | Notes
!! ----------------------------------|------------|-----------------|---------------|---------------------------------|--------------------------------------
!! ice_fraction                      | 1          | aice            | ice_state.F90 | concentration of ice            | |
!! ice_mask                          |            | hm              | ice_grid.F90  | ice mask                        | 0.0 indicates land cell and 1.0 indicates ocean cell
!! inst_ice_ir_dif_albedo            | 1          | alidf           | ice_flux.F90  | near infrared diffuse albedo    | |
!! inst_ice_ir_dir_albedo            | 1          | alidr           | ice_flux.F90  | near infrared direct albedo     | |
!! inst_ice_vis_dif_albedo           | 1          | alvdf           | ice_flux.F90  | visible diffuse albedo          | |
!! inst_ice_vis_dir_albedo           | 1          | advdr           | ice_flux.F90  | visible direct albedo           | |
!! mean_evap_rate_atm_into_ice       | kg m-2 s-1 | evap            | ice_flux.F90  | evaporative water flux          | |
!! mean_fresh_water_to_ocean_rate    | kg m-2 s-1 | fresh           | ice_flux.F90  | fresh water flux to ocean       | |
!! mean_ice_volume                   | m          | vice            | ice_state.F90 | volume of ice per unit area     | |
!! mean_laten_heat_flx_atm_into_ice  | W m-2      | flat            | ice_flux.F90  | latent heat flux                | |
!! mean_net_sw_ir_dif_flx            | W m-2      | fswthruidf      | ice_flux.F90  | near infrared diffuse shortwave penetrating to ocean  | |
!! mean_net_sw_ir_dir_flx            | W m-2      | fswthruidr      | ice_flux.F90  | near infrared direct shortwave penetrating to ocean   | |
!! mean_net_sw_vis_dif_flx           | W m-2      | fswthruvdf      | ice_flux.F90  | visible diffuse shortwave penetrating to ocean        | |
!! mean_net_sw_vis_dir_flx           | W m-2      | fswthruvdr      | ice_flux.F90  | visible direct shortwave penetrating to ocean         | |
!! mean_salt_rate                    | kg m-2 s-1 | fsalt           | ice_flux.F90  | salt flux to ocean              | |
!! mean_sensi_heat_flx_atm_into_ice  | W m-2      | fsens           | ice_flux.F90  | sensible heat flux              | |
!! mean_snow_volume                  | m          | vsno            | ice_state.F90 | volume of snow per unit area    | |
!! mean_sw_pen_to_ocn                | W m-2      | fswthru         | ice_flux.F90  | shortwave penetrating to ocean  | |
!! mean_up_lw_flx_ice                | W m-2      | flwout          | ice_flux.F90  | outgoing longwave radiation     | average over ice fraction only
!! net_heat_flx_to_ocn               | W m-2      | fhocn           | ice_flux.F90  | net heat flux to ocean          | |
!! sea_ice_temperature               | K          | trcr            | ice_state.F90 | surface temperature of ice/snow | Celcius converted to Kelvin for export
!! stress_on_air_ice_merid           | N m-2      | strairyT        | ice_flux.F90  | y component of stress on ice by air  | [vector rotation applied] (@ref VectorRotations)
!! stress_on_air_ice_zonal           | N m-2      | strairxT        | ice_flux.F90  | x component of stress on ice by air  | [vector rotation applied] (@ref VectorRotations)
!! stress_on_ocn_ice_merid           | N m-2      | strocnyT        | ice_flux.F90  | y component of stress on ice by ocean  | [vector rotation applied] (@ref VectorRotations)
!! stress_on_ocn_ice_zonal           | N m-2      | strocnxT        | ice_flux.F90  | x component of stress on ice by ocean  | [vector rotation applied] (@ref VectorRotations)
!!
!! @subsection MemoryManagement Memory Management
!!
!! For coupling fields, the CICE cap has the capability to either reference the internal
!! CICE data arrays directly, or to allow ESMF to allocate separate memory for the
!! coupling fields.  Currently, the CICE cap is set up not to reference internal data
!! arrays directly.  During the [ModelAdvance_Slow] (@ref cice_cap_mod::modeladvance_slow)
!! phase, fields are copied from the import state into CICE internal data arrays.
!! After the model integration timestep completes, internal data arrays are copied
!! into the export state so they can be transferred for coupling.
!!
!! @subsection IO I/O
!!
!! The CICE cap implements a subroutine called `dumpCICEInternal` that writes
!! the fields from ice_flux.F90 to NetCDF files. This is an optional diagnostic subroutine that
!! allows the modeler to check the value of the fields and have a better idea of
!! the state of CICE at each coupling time interval. This is useful to determine the impact of import and export
!! Field connections made on the CICE model. Writing out of these fields during the
!! run phase is controlled by the ESMF Attribute "DumpFields." This is read in
!! during [InitializeP0] (@ref cice_cap_mod::initializep0).
!!
!! @section BuildingAndInstalling Building and Installing
!!
!! Of the two independent makefiles, makefile.nuopc links in the ESMF/NUOPC library
!! and LANL CICE library from external environment variables inherited from an external
!! build system to create liblanl_cice.a; makefile is a generic makefile that
!! allows liblanl_cice.a to be built with user customizable settings.
!! The build system should pass pre-defined LANL CICE library
!! and include path and INSTALLDIR settings into makefile.nuopc. Upon completion,
!! liblanl_cice.a and cice_cap.mod will be copied to the INSTALLDIR and can
!! be linked into a coupled application with other components.
!!
!! @subsection Dependencies Dependencies
!!
!! The CICE cap is only dependent on the CICE model itself, liblanl_cice.a.
!!
!! @section RuntimeConfiguration Runtime Configuration
!!
!! CICE cap accepts two runtime configuration parameters (DumpFields and ProfileMemory)
!! though component level ESMF Attribute settings.
!! By default, DumpFields is true. When turned on to true, diagnose NetCDF files
!! of incoming and outgoing fields will be written.
!! By default, ProfileMemory is true. When turned on to true, run time memory
!! profiling information will be written to ESMF log files.
!!
!! @section Repository
!! The CICE NUOPC cap is maintained in a GitHub repository:
!! https://github.com/feiliuesmf/lanl_cice_cap
!!
!! @section References
!!
!! - [Los Alamos CICE Home Page] (http://oceans11.lanl.gov/trac/CICE)
!! - [CICE User's Guide] (http://oceans11.lanl.gov/trac/CICE/attachment/wiki/WikiStart/cicedoc.pdf?format=raw)
!!
!!
module cice_cap_mod

  use ice_blocks, only: nx_block, ny_block, nblocks_tot, block, get_block, &
                        get_block_parameter
  use ice_domain_size, only: max_blocks, nx_global, ny_global
  use ice_domain, only: nblocks, blocks_ice, distrb_info
  use ice_distribution, only: ice_distributiongetblockloc
  use ice_constants, only: Tffresh, rad_to_deg
  use ice_calendar,  only: dt
  use ice_flux
  use ice_grid, only: TLAT, TLON, ULAT, ULON, hm, tarea, ANGLET, ANGLE, &
                      dxt, dyt, t2ugrid_vector
  use ice_state
  use CICE_RunMod
  use CICE_InitMod
  use CICE_FinalMod 

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS      => SetServices, &
    model_label_SetClock  => label_SetClock, &
    model_label_Advance   => label_Advance, &
    model_label_Finalize  => label_Finalize

  implicit none
  private
  public SetServices

  type cice_internalstate_type
  end type

  type cice_internalstate_wrapper
    type(cice_internalstate_type), pointer :: ptr
  end type

  integer   :: import_slice = 0
  integer   :: export_slice = 0

  type fld_list_type
    character(len=64) :: stdname
    character(len=64) :: shortname
    character(len=64) :: transferOffer
    logical           :: assoc    ! is the farrayPtr associated with internal data
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr
  end type fld_list_type

  integer,parameter :: fldsMax = 100
  integer :: fldsToIce_num = 0
  type (fld_list_type) :: fldsToIce(fldsMax)
  integer :: fldsFrIce_num = 0
  type (fld_list_type) :: fldsFrIce(fldsMax)

  integer :: lsize    ! local number of gridcells for coupling
  character(len=256) :: tmpstr
  character(len=2048):: info
  logical :: isPresent
  integer :: dbrc     ! temporary debug rc value

  type(ESMF_Grid), save :: ice_grid_i
  logical :: write_diagnostics = .true.
  logical :: profile_memory = .true.

  contains
  !-----------------------------------------------------------------------
  !------------------- CICE code starts here -----------------------
  !-----------------------------------------------------------------------

  !> NUOPC SetService method is the only public entry point.
  !! SetServices registers all of the user-provided subroutines
  !! in the module with the NUOPC layer.
  !!
  !! @param gcomp an ESMF_GridComp object
  !! @param rc return code
  subroutine SetServices(gcomp, rc)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    character(len=*),parameter  :: subname='(cice_cap:SetServices)'

    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing method(s)
    ! No need to change clock settings
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance_slow, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=cice_model_finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call CICE_FieldsSetup()

  end subroutine

  !-----------------------------------------------------------------------------

  !> First initialize subroutine called by NUOPC.  The purpose
  !! is to set which version of the Initialize Phase Definition (IPD)
  !! to use.
  !!
  !! For this CICE cap, we are using IPDv01.
  !!
  !! @param gcomp an ESMF_GridComp object
  !! @param importState an ESMF_State object for import fields
  !! @param exportState an ESMF_State object for export fields
  !! @param clock an ESMF_Clock object
  !! @param rc return code
  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    character(len=10)     :: value
    type(ESMF_VM)         :: vm
    integer               :: lpet

    rc = ESMF_SUCCESS

    ! Switch to IPDv01 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv01p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, localPet=lpet, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_AttributeGet(gcomp, name="DumpFields", value=value, defaultValue="true", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    write_diagnostics=(trim(value)=="true")

    call ESMF_AttributeGet(gcomp, name="ProfileMemory", value=value, defaultValue="true", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    profile_memory=(trim(value)/="false")

    if(lpet == 0) &
      print *, 'CICE DumpFields = ', write_diagnostics, 'ProfileMemory = ', profile_memory
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  !> Called by NUOPC to advertise import and export fields.  "Advertise"
  !! simply means that the standard names of all import and export
  !! fields are supplied.  The NUOPC layer uses these to match fields
  !! between components in the coupled system.
  !!
  !! @param gcomp an ESMF_GridComp object
  !! @param importState an ESMF_State object for import fields
  !! @param exportState an ESMF_State object for export fields
  !! @param clock an ESMF_Clock object
  !! @param rc return code
  subroutine InitializeAdvertise(gcomp, importState, exportState, clock, rc)

    type(ESMF_GridComp)                    :: gcomp
    type(ESMF_State)                       :: importState, exportState
    type(ESMF_Clock)                       :: clock
    integer, intent(out)                   :: rc

    ! Local Variables
    type(ESMF_VM)                          :: vm
    integer                                :: mpi_comm
    character(len=*),parameter  :: subname='(cice_cap:InitializeAdvertise)'

    rc = ESMF_SUCCESS

    call ESMF_VMGetCurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(vm, mpiCommunicator=mpi_comm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call CICE_Initialize(mpi_comm)

    call CICE_AdvertiseFields(importState, fldsToIce_num, fldsToIce, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call CICE_AdvertiseFields(exportState, fldsFrIce_num, fldsFrIce, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    write(info,*) subname,' --- initialization phase 1 completed --- '
    call ESMF_LogWrite(info, ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine
  
  !-----------------------------------------------------------------------------

  !> Called by NUOPC to realize import and export fields.  "Realizing" a field
  !! means that its grid has been defined and an ESMF_Field object has been
  !! created and put into the import or export State.
  !!
  !! CICE defines its own parallel decomposition in 2D blocks. The indices
  !! are stored in i_glob and j_glob. These are used to create an ESMF_DistGrid
  !! object using the deBlockList parameter.  The ESMF_DistGrid is set up with
  !! a periodic boundary condition in the first (X) dimension and bipolar
  !! boundary condition at the top row. These are set using calls to
  !! ESMF_DistGridConnectionSet.  Information about how the decomposition
  !! blocks are distributed among processors is determined by a call to
  !! ice_distributionGetBlockLoc().
  !!
  !! Geographic coordinates are added to the ESMF_Grid object at center
  !! and corner stagger locations.  CICE maintains these coordinates in
  !! radians in the TLON, TLAT, ULON, and ULAT arrays.  They are converted
  !! to degrees before adding to the ESMF_Grid object.  Cell areas are
  !! set based on the CICE tarea array and cell masks are set based on
  !! the hm array.
  !!
  !! The fields to import and export are stored in the fldsToIce and fldsFrIce
  !! arrays, respectively.  Each field entry includes the standard name,
  !! information about whether the field's grid will be provided by the cap,
  !! and optionally a pointer to the field's data array.  Currently, all fields
  !! are defined on the same grid defined by the cap (so set to "will provide").
  !! The fields are created by calling cice_cap_mod::cice_realizefields.
  !!
  !! @param gcomp an ESMF_GridComp object
  !! @param importState an ESMF_State object for import fields
  !! @param exportState an ESMF_State object for export fields
  !! @param clock an ESMF_Clock object
  !! @param rc return code
  subroutine InitializeRealize(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! Local Variables
    type(ESMF_VM)                          :: vm
    type(ESMF_Grid)                        :: gridIn
    type(ESMF_Grid)                        :: gridOut
    type(ESMF_DistGrid)                    :: distgrid
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    integer                                :: npet
    integer                                :: i,j,iblk, n, i1,j1, DE
    integer                                :: ilo,ihi,jlo,jhi
    integer                                :: ig,jg,cnt
    integer                                :: peID,locID
    integer, pointer                       :: indexList(:)
    integer, pointer                       :: deLabelList(:)
    integer, pointer                       :: deBlockList(:,:,:)
    integer, pointer                       :: petMap(:)
    integer, pointer                       :: i_glob(:),j_glob(:)
    integer                                :: lbnd(2),ubnd(2)
    type(block)                            :: this_block
    type(ESMF_DELayout)                    :: delayout
    real(ESMF_KIND_R8), pointer            :: tarray(:,:)     
    real(ESMF_KIND_R8), pointer :: coordXcenter(:,:)
    real(ESMF_KIND_R8), pointer :: coordYcenter(:,:)
    real(ESMF_KIND_R8), pointer :: coordXcorner(:,:)
    real(ESMF_KIND_R8), pointer :: coordYcorner(:,:)
    integer(ESMF_KIND_I4), pointer :: gridmask(:,:)
    real(ESMF_KIND_R8), pointer :: gridarea(:,:)
    character(len=*),parameter  :: subname='(cice_cap:InitializeRealize)'

    rc = ESMF_SUCCESS

    ! We can check if npet is 4 or some other value to make sure
    ! CICE is configured to run on the correct number of processors.

    ! create a Grid object for Fields
    ! we are going to create a single tile displaced pole grid from a gridspec
    ! file. We also use the exact decomposition in CICE so that the Fields
    ! created can wrap on the data pointers in internal part of CICE

    write(tmpstr,'(a,2i8)') subname//' ice nx,ny = ',nx_global,ny_global
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

!    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/nx_global,ny_global/), &
!       regDecomp=(/2,2/), rc=rc)

    allocate(deBlockList(2,2,nblocks_tot))
    allocate(petMap(nblocks_tot))
    allocate(deLabelList(nblocks_tot))

    write(tmpstr,'(a,1i8)') subname//' nblocks = ',nblocks_tot
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    do n = 1, nblocks_tot
       deLabelList(n) = n
       call get_block_parameter(n,ilo=ilo,ihi=ihi,jlo=jlo,jhi=jhi, &
          i_glob=i_glob,j_glob=j_glob)
       deBlockList(1,1,n) = i_glob(ilo)
       deBlockList(1,2,n) = i_glob(ihi)
       deBlockList(2,1,n) = j_glob(jlo)
       deBlockList(2,2,n) = j_glob(jhi)
       call ice_distributionGetBlockLoc(distrb_info,n,peID,locID)
       petMap(n) = peID - 1
       write(tmpstr,'(a,2i8)') subname//' IDs  = ',n,peID
       call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
       write(tmpstr,'(a,3i8)') subname//' iglo = ',n,deBlockList(1,1,n),deBlockList(1,2,n)
       call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
       write(tmpstr,'(a,3i8)') subname//' jglo = ',n,deBlockList(2,1,n),deBlockList(2,2,n)
       call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo

    delayout = ESMF_DELayoutCreate(petMap, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    allocate(connectionList(2))
    ! bipolar boundary condition at top row: nyg
    call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1, &
      tileIndexB=1, positionVector=(/nx_global+1, 2*ny_global+1/), &
      orientationVector=(/-1, -2/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! periodic boundary condition along first dimension
    call ESMF_DistGridConnectionSet(connectionList(2), tileIndexA=1, &
      tileIndexB=1, positionVector=(/nx_global, 0/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/nx_global,ny_global/), &
!        indexflag = ESMF_INDEX_DELOCAL, &
        deBlockList=deBlockList, &
!        deLabelList=deLabelList, &
        delayout=delayout, &
        connectionList=connectionList, &
        rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    deallocate(deLabelList)
    deallocate(deBlockList)
    deallocate(petMap)
    deallocate(connectionList)

!    call ESMF_DistGridPrint(distgrid=distgrid, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

    call ESMF_DistGridGet(distgrid=distgrid, localDE=0, elementCount=cnt, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    allocate(indexList(cnt))
    write(tmpstr,'(a,i8)') subname//' distgrid cnt= ',cnt
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    call ESMF_DistGridGet(distgrid=distgrid, localDE=0, seqIndexList=indexList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    write(tmpstr,'(a,4i8)') subname//' distgrid list= ',indexList(1),indexList(cnt),minval(indexList), maxval(indexList)
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    deallocate(IndexList)

!    gridIn = ESMF_GridCreate('global_gx3_gridspec.nc', ESMF_FILEFORMAT_GRIDSPEC, &
!!      (/2,2/), isSphere=.true., coordNames=(/'ulon', 'ulat'/), &
!      distgrid=distgrid, isSphere=.true., coordNames=(/'ulon', 'ulat'/), &
!      indexflag=ESMF_INDEX_DELOCAL, addCornerStagger=.true., rc=rc)
!!      indexflag=ESMF_INDEX_GLOBAL, addCornerStagger=.true., rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

    gridIn = ESMF_GridCreate(distgrid=distgrid, &
       coordSys = ESMF_COORDSYS_SPH_DEG, &
       gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
       rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    call ESMF_GridAddCoord(gridIn, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_GridAddCoord(gridIn, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_GridAddItem(gridIn, itemFlag=ESMF_GRIDITEM_MASK, itemTypeKind=ESMF_TYPEKIND_I4, &
       staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_GridAddItem(gridIn, itemFlag=ESMF_GRIDITEM_AREA, itemTypeKind=ESMF_TYPEKIND_R8, &
       staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    do iblk = 1,nblocks
       DE = iblk-1
       this_block = get_block(blocks_ice(iblk),iblk)
       ilo = this_block%ilo
       ihi = this_block%ihi
       jlo = this_block%jlo
       jhi = this_block%jhi

       call ESMF_GridGetCoord(gridIn, coordDim=1, localDE=DE, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           computationalLBound=lbnd, computationalUBound=ubnd, &
           farrayPtr=coordXcenter, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       call ESMF_GridGetCoord(gridIn, coordDim=2, localDE=DE, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=coordYcenter, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

       write(tmpstr,'(a,5i8)') subname//' iblk center bnds ',iblk,lbnd,ubnd
       call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
       if (lbnd(1) /= 1 .or. lbnd(2) /= 1 .or. ubnd(1) /= ihi-ilo+1 .or. ubnd(2) /= jhi-jlo+1) then
          write(tmpstr,'(a,5i8)') subname//' iblk bnds ERROR '
          call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)
          rc = ESMF_FAILURE
          return
       endif

       call ESMF_GridGetItem(gridIn, itemflag=ESMF_GRIDITEM_MASK, localDE=DE, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=gridmask, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       call ESMF_GridGetItem(gridIn, itemflag=ESMF_GRIDITEM_AREA, localDE=DE, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=gridarea, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

       do j1 = lbnd(2),ubnd(2)
       do i1 = lbnd(1),ubnd(1)
          i = i1 + ilo - lbnd(1)
          j = j1 + jlo - lbnd(2)
          coordXcenter(i1,j1) = TLON(i,j,iblk) * rad_to_deg
          coordYcenter(i1,j1) = TLAT(i,j,iblk) * rad_to_deg
          gridmask(i1,j1) = nint(hm(i,j,iblk))
          gridarea(i1,j1) = tarea(i,j,iblk)
       enddo
       enddo

       call ESMF_GridGetCoord(gridIn, coordDim=1, localDE=DE, &
           staggerloc=ESMF_STAGGERLOC_CORNER, &
           computationalLBound=lbnd, computationalUBound=ubnd, &
           farrayPtr=coordXcorner, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
       call ESMF_GridGetCoord(gridIn, coordDim=2, localDE=DE, &
           staggerloc=ESMF_STAGGERLOC_CORNER, &
           farrayPtr=coordYcorner, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

       write(tmpstr,'(a,5i8)') subname//' iblk corner bnds ',iblk,lbnd,ubnd
       call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

       ! ULON and ULAT are upper right hand corner from TLON and TLAT
       ! corners in ESMF need to be defined lon lower left corner from center
       ! ULON and ULAT have ghost cells, leverage that to fill corner arrays
       do j1 = lbnd(2),ubnd(2)
       do i1 = lbnd(1),ubnd(1)
          i = i1 + ilo - lbnd(1)
          j = j1 + jlo - lbnd(2)
          coordXcorner(i1,j1) = ULON(i-1,j-1,iblk) * rad_to_deg
          coordYcorner(i1,j1) = ULAT(i-1,j-1,iblk) * rad_to_deg
       enddo
       enddo

    enddo

    call ESMF_GridGetCoord(gridIn, coordDim=1, localDE=0,  &
       staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=tarray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    write(tmpstr,'(a,2g15.7)') subname//' gridIn center1 = ',minval(tarray),maxval(tarray)
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_GridGetCoord(gridIn, coordDim=2, localDE=0,  &
       staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=tarray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    write(tmpstr,'(a,2g15.7)') subname//' gridIn center2 = ',minval(tarray),maxval(tarray)
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_GridGetCoord(gridIn, coordDim=1, localDE=0,  &
       staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=tarray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    write(tmpstr,'(a,2g15.7)') subname//' gridIn corner1 = ',minval(tarray),maxval(tarray)
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

    call ESMF_GridGetCoord(gridIn, coordDim=2, localDE=0,  &
       staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=tarray, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    write(tmpstr,'(a,2g15.7)') subname//' gridIn corner2 = ',minval(tarray),maxval(tarray)
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

    gridOut = gridIn ! for now out same as in
    ice_grid_i = gridIn

    call CICE_RealizeFields(importState, gridIn , fldsToIce_num, fldsToIce, "Ice import", rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call CICE_RealizeFields(exportState, gridOut, fldsFrIce_num, fldsFrIce, "Ice export", rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

! Have to be careful with reset since states are pointing directly into cice arrays
!    call state_reset(ImportState, value=-99._ESMF_KIND_R8, rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out
    call state_reset(ExportState, value=-99._ESMF_KIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

!    call State_getFldPtr(exportState,'ifrac'    ,dataPtr_ifrac,rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
!    call State_getFldPtr(exportState,'sit'      ,dataPtr_itemp,rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
!    dataPtr_ifrac = -99._ESMF_KIND_R8
!    dataPtr_itemp = -99._ESMF_KIND_R8

    write(info,*) subname,' --- initialization phase 2 completed --- '
    call ESMF_LogWrite(info, ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=dbrc)

  end subroutine
  
  !-----------------------------------------------------------------------------

  !> NUOPC specialization point to set up this cap's clock.
  !!
  !! By default, the clock is a copy of the clock coming in from the NUOPC
  !! Driver.  In this case we are pulling in the thermodynamic timestep
  !! from the CICE model (dt) an setting that as the "stability timestep."
  !! This ensures that if dt is smaller than the Driver's requested timestep
  !! length, it will use dt instead.  In this case, subcycling is automatically
  !! handled by NUOPC, e.g., the CICE model will potentially be called multiple
  !! times until it reaches the coupling timestep requested by the Driver.
  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep, timestep
    character(len=*),parameter  :: subname='(cice_cap:SetClock)'

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! tcraig: dt is the cice thermodynamic timestep in seconds
    call ESMF_TimeIntervalSet(timestep, s=nint(dt), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockSet(clock, timestep=timestep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    call ESMF_TimeIntervalSet(stabilityTimeStep, s=nint(dt), rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetClock(gcomp, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------

  !> Called by NUOPC to advance the CICE model a single timestep.
  !!
  !! This subroutine copies field data out of the cap import state and into the
  !! model internal arrays.  Then it calls CICE_Run to make a single timestep.
  !! Finally, it copies the updated arrays into the cap export state.
  !!
  !! This routine also writes out a diagnostic file for each import and export
  !! field using NUOPC_FieldWrite.  The files are named field_ice_import_<fldname>.nc
  !! and field_ice_export_<fldname>.nc, respectively.  In this subroutine there
  !! are numerous calls to cice_cap_mod::state_getfldptr which is a convenience function to retrieve
  !! the data pointer from an ESMF_State object based on the field's name.
  !!
  !! All incoming vectors from mediator are rotated counter clock wise onto local basis vector directions.
  !! All outgoing vectors to mediator are rotated clock wise onto regular lat-lon directions.
  !! Zonal and meridional sea surface slope fields are calculated from sea level input and
  !! rotated counter clock wise onto local basis vector directions. Internally LANL cice
  !! uses the zonal and meridional sea surface slope fields calculated here.
  !!
  !! At the end of the subroutine, a series of calls is made to cice_cap_mod::dumpciceinternal
  !! which writes out a time slice of each CICE internal variable for diagnostic purposes.
  !!
  !! @param gcomp an ESMF_GridComp object
  !! @param rc return code
  subroutine ModelAdvance_slow(gcomp, rc)
    type(ESMF_GridComp)                    :: gcomp
    integer, intent(out)                   :: rc
    
    ! local variables
    type(ESMF_Clock)                       :: clock
    type(ESMF_State)                       :: importState, exportState
    type(ESMF_Time)                        :: currTime
    type(ESMF_TimeInterval)                :: timeStep
    type(ESMF_Field)                       :: lfield,lfield2d
    type(ESMF_Grid)                        :: grid
    real(ESMF_KIND_R8), pointer            :: fldptr(:,:,:)
    real(ESMF_KIND_R8), pointer            :: fldptr2d(:,:)
    type(block)                            :: this_block
    character(len=64)                      :: fldname
    integer                                :: i,j,iblk,n,i1,i2,j1,j2
    integer                                :: ilo,ihi,jlo,jhi
    real(ESMF_KIND_R8)                     :: ue, vn, ui, vj
    real(ESMF_KIND_R8)                     :: sigma_r, sigma_l, sigma_c
    type(ESMF_StateItem_Flag)              :: itemType
    ! imports
    real(ESMF_KIND_R8), pointer :: dataPtr_mdlwfx(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_swvr(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_swvf(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_swir(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_swif(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_lprec(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fprec(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_sst(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_sss(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_sl(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_sssz(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_sssm(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_ocncz(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_ocncm(:,:,:)
!    real(ESMF_KIND_R8), pointer :: dataPtr_ocnci(:,:,:)
!    real(ESMF_KIND_R8), pointer :: dataPtr_ocncj(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fmpot(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_mld(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_mzmf(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_mmmf(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_rhoabot(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_Tbot(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_pbot(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_qbot(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_zlvl(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_ubot(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_vbot(:,:,:)
    ! exports
    real(ESMF_KIND_R8), pointer :: dataPtr_mask(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_ifrac(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_itemp(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_alvdr(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_alidr(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_alvdf(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_alidf(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_strairxT(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_strairyT(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_strocnxT(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_strocnyT(:,:,:)
!    real(ESMF_KIND_R8), pointer :: dataPtr_strocni(:,:,:)
!    real(ESMF_KIND_R8), pointer :: dataPtr_strocnj(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fswthru(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fswthruvdr(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fswthruvdf(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fswthruidr(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fswthruidf(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_flwout(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fsens(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_flat(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_evap(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fhocn(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fresh(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_fsalt(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_vice(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_vsno(:,:,:)
    character(240)              :: msgString
    character(len=*),parameter  :: subname='(cice_cap:ModelAdvance_slow)'

    rc = ESMF_SUCCESS
    if(profile_memory) call ESMF_VMLogMemInfo("Entering CICE Model_ADVANCE: ")
    write(info,*) subname,' --- run phase 1 called --- '
    call ESMF_LogWrite(info, ESMF_LOGMSG_INFO, rc=dbrc)

    
    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing CICE from: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_TimePrint(currTime + timeStep, &
      preString="--------------------------------> to: ", &
      unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  if(write_diagnostics) then
    import_slice = import_slice + 1

#if (1 == 0)
!tcx causes core dumps and garbage
    call NUOPC_StateWrite(importState, filePrefix='field_ice_import_', &
      timeslice=import_slice, relaxedFlag=.true., rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#else
    do i = 1,fldsToice_num
      fldname = fldsToice(i)%shortname
      call ESMF_StateGet(importState, itemName=trim(fldname), itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        call ESMF_StateGet(importState, itemName=trim(fldname), field=lfield, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call ESMF_FieldGet(lfield,grid=grid,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        ! create a copy of the 3d data in lfield but in a 2d array, lfield2d
        lfield2d = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, indexflag=ESMF_INDEX_DELOCAL, &
          name=trim(fldname), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call ESMF_FieldGet(lfield  , farrayPtr=fldptr  , rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call ESMF_FieldGet(lfield2d, farrayPtr=fldptr2d, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        fldptr2d(:,:) = fldptr(:,:,1)

! causes core dumps and garbage
!        call NUOPC_Write(lfield, fileName='fieldN3d_ice_import_'//trim(fldname)//'.nc', &
!          timeslice=import_slice, relaxedFlag=.true., rc=rc) 
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!          line=__LINE__, &
!          file=__FILE__)) &
!          return  ! bail out

! causes run time error in usage
!        call NUOPC_Write(lfield2d, fileName='fieldN_ice_import_'//trim(fldname)//'.nc', &
!          timeslice=import_slice, relaxedFlag=.true., rc=rc) 
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!          line=__LINE__, &
!          file=__FILE__)) &
!          return  ! bail out

! causes core dumps and garbage
!        call ESMF_FieldPrint(lfield,rc=rc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!          line=__LINE__, &
!          file=__FILE__)) &
!          return  ! bail out
!        call ESMF_FieldWrite(lfield, fileName='field3d_ice_import_'//trim(fldname)//'.nc', &
!          timeslice=import_slice, rc=rc) 
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!          line=__LINE__, &
!          file=__FILE__)) &
!          return  ! bail out

        call ESMF_FieldWrite(lfield2d, fileName='field_ice_import_'//trim(fldname)//'.nc', &
          timeslice=import_slice, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call ESMF_FieldDestroy(lfield2d, noGarbage=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
    enddo
#endif
  endif  ! write_diagnostics

    call State_getFldPtr(importState,'inst_temp_height_lowest',dataPtr_Tbot,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'inst_spec_humid_height_lowest',dataPtr_qbot,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'inst_zonal_wind_height_lowest',dataPtr_ubot,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'inst_merid_wind_height_lowest',dataPtr_vbot,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'inst_pres_height_lowest',dataPtr_pbot,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'mean_down_lw_flx',dataPtr_mdlwfx,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'mean_down_sw_vis_dir_flx',dataPtr_swvr,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'mean_down_sw_vis_dif_flx',dataPtr_swvf,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'mean_down_sw_ir_dir_flx',dataPtr_swir,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'mean_down_sw_ir_dif_flx',dataPtr_swif,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'mean_prec_rate',dataPtr_lprec,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'mean_fprec_rate',dataPtr_fprec,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'sea_surface_temperature',dataPtr_sst,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'s_surf',dataPtr_sss,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'sea_lev',dataPtr_sl,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'sea_surface_slope_zonal',dataPtr_sssz,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'sea_surface_slope_merid',dataPtr_sssm,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'ocn_current_zonal',dataPtr_ocncz,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'ocn_current_merid',dataPtr_ocncm,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
!    call State_getFldPtr(importState,'ocn_current_idir',dataPtr_ocnci,rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
!    call State_getFldPtr(importState,'ocn_current_jdir',dataPtr_ocncj,rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'freezing_melting_potential',dataPtr_fmpot,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'mixed_layer_depth',dataPtr_mld,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'mean_zonal_moment_flx',dataPtr_mzmf,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'mean_merid_moment_flx',dataPtr_mmmf,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'inst_height_lowest',dataPtr_zlvl,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(importState,'air_density_height_lowest',dataPtr_rhoabot,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return

    do iblk = 1,nblocks
       this_block = get_block(blocks_ice(iblk),iblk)
       ilo = this_block%ilo
       ihi = this_block%ihi
       jlo = this_block%jlo
       jhi = this_block%jhi
       do j = jlo,jhi
       do i = ilo,ihi
          i1 = i - ilo + 1
          j1 = j - jlo + 1
          !rhoa   (i,j,iblk) = dataPtr_ips(i1,j1,iblk)/(287.058*(1+0.608*dataPtr_ishh2m (i1,j1,iblk))*dataPtr_ith2m  (i1,j1,iblk))
          rhoa   (i,j,iblk) = dataPtr_rhoabot(i1,j1,iblk)  ! import directly from mediator  
          potT   (i,j,iblk) = dataPtr_Tbot   (i1,j1,iblk) * (100000./dataPtr_pbot(i1,j1,iblk))**0.286 ! Potential temperature (K)
          Tair   (i,j,iblk) = dataPtr_Tbot   (i1,j1,iblk)  ! near surface temp, maybe lowest level (K)
          Qa     (i,j,iblk) = dataPtr_qbot   (i1,j1,iblk)  ! near surface humidity, maybe lowest level (kg/kg)
          zlvl   (i,j,iblk) = dataPtr_zlvl   (i1,j1,iblk)  ! height of the lowest level (m) 
          flw    (i,j,iblk) = dataPtr_mdlwfx (i1,j1,iblk)  ! downwelling longwave flux
          swvdr  (i,j,iblk) = dataPtr_swvr   (i1,j1,iblk)  ! downwelling shortwave flux, vis dir
          swvdf  (i,j,iblk) = dataPtr_swvf   (i1,j1,iblk)  ! downwelling shortwave flux, vis dif
          swidr  (i,j,iblk) = dataPtr_swir   (i1,j1,iblk)  ! downwelling shortwave flux, nir dir
          swidf  (i,j,iblk) = dataPtr_swif   (i1,j1,iblk)  ! downwelling shortwave flux, nir dif
          fsw(i,j,iblk) = swvdr(i,j,iblk)+swvdf(i,j,iblk)+swidr(i,j,iblk)+swidf(i,j,iblk)
          frain  (i,j,iblk) = dataPtr_lprec  (i1,j1,iblk)  ! flux of rain (liquid only)
          fsnow  (i,j,iblk) = dataPtr_fprec  (i1,j1,iblk)  ! flux of frozen precip ! fprec is all junk values from med, no src
          sss    (i,j,iblk) = dataPtr_sss    (i1,j1,iblk)  ! sea surface salinity (maybe for mushy layer)
! availability of ocean heat content (or freezing potential, use all if freezing) ! can potentially connect but contains junk from med, no src
          sst    (i,j,iblk) = dataPtr_sst    (i1,j1,iblk) - 273.15  ! sea surface temp (may not be needed?)
!!    Ice%bheat : bottom heat conducted up from ocean due to temperaure difference between sst and melting ice
!!    real    :: kmelt          = 6e-5*4e6   ! ocean/ice heat flux constant
!!    real, public, parameter :: TFREEZE = 273.16 
!!    real, parameter :: MU_TS = 0.054     ! relates freezing temp. to salinity
!          frzmlt (i,j,iblk) = -6e-5*4e6*(sst (i,j,iblk) + 0.054*dataPtr_sss(i1,j1,iblk))
!          if(dataPtr_fmpot  (i1,j1,iblk) .gt. 0) frzmlt (i,j,iblk) = dataPtr_fmpot  (i1,j1,iblk)/dt  
! Fei, Let MOM5 take care of frazil calculation 10/5/15 (import dataPtr_fmpot in W/m^2)
          frzmlt (i,j,iblk) = dataPtr_fmpot  (i1,j1,iblk)
!          hmix   (i,j,iblk) = dataPtr_mld    (i1,j1,iblk)  ! ocean mixed layer depth (may not be needed?)
!          ! --- rotate these vectors from east/north to i/j ---
          !ue = dataPtr_mzmf(i1,j1,iblk)
          !vn = dataPtr_mmmf(i1,j1,iblk)
          !strax  (i,j,iblk) = -(ue*cos(ANGLET(i,j,iblk)) + vn*sin(ANGLET(i,j,iblk)))  ! lowest level wind stress or momentum flux (Pa)
          !stray  (i,j,iblk) = -(ue*cos(ANGLET(i,j,iblk)) - vn*sin(ANGLET(i,j,iblk)))  ! lowest level wind stress or momentum flux (Pa)
          ue = dataPtr_ocncz  (i1,j1,iblk)
          vn = dataPtr_ocncm  (i1,j1,iblk)
          uocn   (i,j,iblk) = ue*cos(ANGLET(i,j,iblk)) + vn*sin(ANGLET(i,j,iblk))  ! ocean current
          vocn   (i,j,iblk) = -ue*sin(ANGLET(i,j,iblk)) + vn*cos(ANGLET(i,j,iblk))  ! ocean current
!         uocn   (i,j,iblk) = dataPtr_ocnci  (i1,j1,iblk)
!         vocn   (i,j,iblk) = dataPtr_ocncj  (i1,j1,iblk)
          ue = dataPtr_ubot  (i1,j1,iblk)
          vn = dataPtr_vbot  (i1,j1,iblk)
          uatm   (i,j,iblk) = ue*cos(ANGLET(i,j,iblk)) + vn*sin(ANGLET(i,j,iblk))  ! wind u component
          vatm   (i,j,iblk) = -ue*sin(ANGLET(i,j,iblk)) + vn*cos(ANGLET(i,j,iblk))  ! wind v component
          wind   (i,j,iblk) = sqrt(dataPtr_ubot  (i1,j1,iblk)**2 + dataPtr_vbot  (i1,j1,iblk)**2)     ! wind speed

          ! zonal sea surface slope
          sigma_r = 0.5*(dataPtr_sl(i1+1,j1+1,iblk)-dataPtr_sl(i1,j1+1,iblk)+ dataPtr_sl(i1+1,j1,iblk)-dataPtr_sl(i1,j1,iblk))/dxt(i,j,iblk)
          sigma_l = 0.5*(dataPtr_sl(i1,j1+1,iblk)-dataPtr_sl(i1-1,j1+1,iblk)+ dataPtr_sl(i1,j1,iblk)-dataPtr_sl(i1-1,j1,iblk))/dxt(i,j,iblk)
          sigma_c = 0.5*(sigma_r+sigma_l)
          if ( (sigma_r * sigma_l) .GT. 0.0 ) then
            ss_tltx(i,j,iblk) = sign ( min( 2.*min(abs(sigma_l),abs(sigma_r)), abs(sigma_c) ), sigma_c )
          else
            ss_tltx(i,j,iblk) = 0.0
          endif
          ! meridional sea surface slope
          sigma_r = 0.5*(dataPtr_sl(i1+1,j1+1,iblk)-dataPtr_sl(i1+1,j1,iblk)+ dataPtr_sl(i1,j1+1,iblk)-dataPtr_sl(i1,j1,iblk))/dyt(i,j,iblk)
          sigma_l = 0.5*(dataPtr_sl(i1+1,j1,iblk)-dataPtr_sl(i1+1,j1-1,iblk)+ dataPtr_sl(i1,j1,iblk)-dataPtr_sl(i1,j1-1,iblk))/dyt(i,j,iblk)
          sigma_c = 0.5*(sigma_r+sigma_l)
          if ( (sigma_r * sigma_l) .GT. 0.0 ) then
            ss_tlty(i,j,iblk) = sign ( min( 2.*min(abs(sigma_l),abs(sigma_r)), abs(sigma_c) ), sigma_c )
          else
            ss_tlty(i,j,iblk) = 0.0
          endif
          ! rotate onto local basis vectors
          ue = ss_tltx   (i,j,iblk)
          vn = ss_tlty   (i,j,iblk)
          ss_tltx(i,j,iblk) = ue*cos(ANGLET(i,j,iblk)) + vn*sin(ANGLET(i,j,iblk))
          ss_tlty(i,j,iblk) = -ue*sin(ANGLET(i,j,iblk)) + vn*cos(ANGLET(i,j,iblk))

       enddo
       enddo

       call t2ugrid_vector(ss_tltx)
       call t2ugrid_vector(ss_tlty)
    enddo

    write(info,*) subname,' --- run phase 2 called --- '
    call ESMF_LogWrite(info, ESMF_LOGMSG_INFO, rc=dbrc)
    if(profile_memory) call ESMF_VMLogMemInfo("Before CICE_Run")
    call CICE_Run
    if(profile_memory) call ESMF_VMLogMemInfo("Afterr CICE_Run")
    write(info,*) subname,' --- run phase 3 called --- '
    call ESMF_LogWrite(info, ESMF_LOGMSG_INFO, rc=dbrc)

    !---- local modifications to coupling fields -----

    call State_getFldPtr(exportState,'ice_mask',dataPtr_mask,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'ice_fraction',dataPtr_ifrac,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'sea_ice_temperature',dataPtr_itemp,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'inst_ice_vis_dir_albedo',dataPtr_alvdr,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'inst_ice_vis_dif_albedo',dataPtr_alvdf,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'inst_ice_ir_dir_albedo',dataPtr_alidr,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'inst_ice_ir_dif_albedo',dataPtr_alidf,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'stress_on_air_ice_zonal',dataPtr_strairxT,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'stress_on_air_ice_merid',dataPtr_strairyT,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'stress_on_ocn_ice_zonal',dataPtr_strocnxT,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'stress_on_ocn_ice_merid',dataPtr_strocnyT,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
!    call State_getFldPtr(exportState,'stress_on_ocn_ice_idir',dataPtr_strocni,rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
!    call State_getFldPtr(exportState,'stress_on_ocn_ice_jdir',dataPtr_strocnj,rc=rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'net_heat_flx_to_ocn',dataPtr_fhocn,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_fresh_water_to_ocean_rate',dataPtr_fresh,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_salt_rate',dataPtr_fsalt,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_ice_volume',dataPtr_vice,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_snow_volume',dataPtr_vsno,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_sw_pen_to_ocn',dataPtr_fswthru,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_net_sw_vis_dir_flx',dataPtr_fswthruvdr,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_net_sw_vis_dif_flx',dataPtr_fswthruvdf,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_net_sw_ir_dir_flx',dataPtr_fswthruidr,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_net_sw_ir_dif_flx',dataPtr_fswthruidf,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_up_lw_flx_ice',dataPtr_flwout,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_sensi_heat_flx_atm_into_ice',dataPtr_fsens,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_laten_heat_flx_atm_into_ice',dataPtr_flat,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return
    call State_getFldPtr(exportState,'mean_evap_rate_atm_into_ice',dataPtr_evap,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)) return

    write(info, *) subname//' ifrac size :', &
      lbound(dataPtr_ifrac,1), ubound(dataPtr_ifrac,1), &
      lbound(dataPtr_ifrac,2), ubound(dataPtr_ifrac,2), &
      lbound(dataPtr_ifrac,3), ubound(dataPtr_ifrac,3)
    call ESMF_LogWrite(info, ESMF_LOGMSG_INFO, rc=dbrc)

    dataPtr_ifrac = 0._ESMF_KIND_R8
    dataPtr_itemp = 0._ESMF_KIND_R8
    dataPtr_mask = 0._ESMF_KIND_R8
    do iblk = 1,nblocks
       this_block = get_block(blocks_ice(iblk),iblk)
       ilo = this_block%ilo
       ihi = this_block%ihi
       jlo = this_block%jlo
       jhi = this_block%jhi
       do j = jlo,jhi
       do i = ilo,ihi
          i1 = i - ilo + 1
          j1 = j - jlo + 1
          if (hm(i,j,iblk) > 0.5) dataPtr_mask(i1,j1,iblk) = 1._ESMF_KIND_R8
          dataPtr_ifrac   (i1,j1,iblk) = aice(i,j,iblk)   ! ice fraction (0-1)
          if (dataPtr_ifrac(i1,j1,iblk) > 0._ESMF_KIND_R8) &
             dataPtr_itemp   (i1,j1,iblk) = Tffresh + trcr(i,j,1,iblk)  ! surface temperature of ice covered portion (degK)
          dataPtr_alvdr   (i1,j1,iblk) = alvdr(i,j,iblk)  ! albedo vis dir
          dataPtr_alidr   (i1,j1,iblk) = alidr(i,j,iblk)  ! albedo nir dir
          dataPtr_alvdf   (i1,j1,iblk) = alvdf(i,j,iblk)  ! albedo vis dif
          dataPtr_alidf   (i1,j1,iblk) = alidf(i,j,iblk)  ! albedo nir dif
          dataPtr_fswthru (i1,j1,iblk) = fswthru(i,j,iblk) ! flux of shortwave through ice to ocean
          dataPtr_fswthruvdr (i1,j1,iblk) = fswthruvdr(i,j,iblk) ! flux of vis dir shortwave through ice to ocean
          dataPtr_fswthruvdf (i1,j1,iblk) = fswthruvdf(i,j,iblk) ! flux of vis dif shortwave through ice to ocean
          dataPtr_fswthruidr (i1,j1,iblk) = fswthruidr(i,j,iblk) ! flux of ir dir shortwave through ice to ocean
          dataPtr_fswthruidf (i1,j1,iblk) = fswthruidf(i,j,iblk) ! flux of ir dif shortwave through ice to ocean
! could change this to be total gridcell fluxes including the ocean, this would imply atm-ocean
!   fluxes are computed here.  requires some minor changes in cice to do that.
!   turn on slab ocean coupling.
! important scientifically to compute surface heat fluxes in ocean and ice separately and even in each ice category separately.
! fluxes might be weighted by ice fraction already, need to check.
! need meltwater sent to the ocean?
! need heat potential taken up from the ocean?  related to frzmlt.  (always = if freezing, <= if melting)
          dataPtr_flwout  (i1,j1,iblk) = flwout(i,j,iblk) ! longwave outgoing (upward), average over ice fraction only
          dataPtr_fsens   (i1,j1,iblk) = fsens(i,j,iblk)  ! sensible
          dataPtr_flat    (i1,j1,iblk) = flat(i,j,iblk)   ! latent
          dataPtr_evap    (i1,j1,iblk) = evap(i,j,iblk)   ! evaporation (not ~latent, need separate field)
          dataPtr_fhocn    (i1,j1,iblk) = fhocn(i,j,iblk)   ! heat exchange with ocean 
          dataPtr_fresh    (i1,j1,iblk) = fresh(i,j,iblk)   ! fresh water to ocean
          dataPtr_fsalt    (i1,j1,iblk) = fsalt(i,j,iblk)   ! salt to ocean
          dataPtr_vice    (i1,j1,iblk) = vice(i,j,iblk)   ! sea ice volume
          dataPtr_vsno    (i1,j1,iblk) = vsno(i,j,iblk)   ! snow volume
          ! --- rotate these vectors from i/j to east/north ---
          ui = strairxT(i,j,iblk)
          vj = strairyT(i,j,iblk)
          dataPtr_strairxT(i1,j1,iblk) = ui*cos(ANGLET(i,j,iblk)) - vj*sin(ANGLET(i,j,iblk))  ! air ice stress
          dataPtr_strairyT(i1,j1,iblk) = ui*sin(ANGLET(i,j,iblk)) + vj*cos(ANGLET(i,j,iblk))  ! air ice stress
          ui = -strocnxT(i,j,iblk)
          vj = -strocnyT(i,j,iblk)
          dataPtr_strocnxT(i1,j1,iblk) = ui*cos(ANGLET(i,j,iblk)) - vj*sin(ANGLET(i,j,iblk))  ! ice ocean stress
          dataPtr_strocnyT(i1,j1,iblk) = ui*sin(ANGLET(i,j,iblk)) + vj*cos(ANGLET(i,j,iblk))  ! ice ocean stress
!          dataPtr_strocni(i1,j1,iblk) = ui
!          dataPtr_strocnj(i1,j1,iblk) = vj
!!          write(tmpstr,'(a,3i6,2x,g17.7)') subname//' aice = ',i,j,iblk,dataPtr_ifrac(i,j,iblk)
!!          call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
       enddo
       enddo
    enddo

    write(tmpstr,*) subname//' mask = ',minval(dataPtr_mask),maxval(dataPtr_mask)
    call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)

    !-------------------------------------------------

    call state_diagnose(exportState, 'cice_export', rc)

  if(write_diagnostics) then
    export_slice = export_slice + 1

#if (1 == 0)
!tcx causes core dumps and garbage
    call NUOPC_StateWrite(exportState, filePrefix='field_ice_export_', &
      timeslice=export_slice, relaxedFlag=.true., rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#else
    do i = 1,fldsFrIce_num
      fldname = fldsFrIce(i)%shortname
      call ESMF_StateGet(exportState, itemName=trim(fldname), itemType=itemType, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
        call ESMF_StateGet(exportState, itemName=trim(fldname), field=lfield, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call ESMF_FieldGet(lfield,grid=grid,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        ! create a copy of the 3d data in lfield but in a 2d array, lfield2d
        lfield2d = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, indexflag=ESMF_INDEX_DELOCAL, &
          name=trim(fldname), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call ESMF_FieldGet(lfield  , farrayPtr=fldptr  , rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        call ESMF_FieldGet(lfield2d, farrayPtr=fldptr2d, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        fldptr2d(:,:) = fldptr(:,:,1)

! causes core dumps and garbage
!        call NUOPC_Write(lfield, fileName='field_ice_export_'//trim(fldname)//'.nc', &
!          timeslice=export_slice, relaxedFlag=.true., rc=rc) 
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!          line=__LINE__, &
!          file=__FILE__)) &
!          return  ! bail out

        call ESMF_FieldWrite(lfield2d, fileName='field_ice_export_'//trim(fldname)//'.nc', &
          timeslice=export_slice, rc=rc) 
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        call ESMF_FieldDestroy(lfield2d, noGarbage=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif
    enddo
#endif
  endif  ! write_diagnostics

    write(info,*) subname,' --- run phase 4 called --- ',rc
    call ESMF_LogWrite(info, ESMF_LOGMSG_INFO, rc=dbrc)

! Dump out all the cice internal fields to cross-examine with those connected with mediator
! This will help to determine roughly which fields can be hooked into cice

   call dumpCICEInternal(ice_grid_i, import_slice, "inst_zonal_wind_height10m", "will provide", strax)
   call dumpCICEInternal(ice_grid_i, import_slice, "inst_merid_wind_height10m", "will provide", stray)
   call dumpCICEInternal(ice_grid_i, import_slice, "inst_pres_height_surface" , "will provide", zlvl)
   call dumpCICEInternal(ice_grid_i, import_slice, "xx_pot_air_temp"          , "will provide", potT)
   call dumpCICEInternal(ice_grid_i, import_slice, "inst_temp_height2m"       , "will provide", Tair)
   call dumpCICEInternal(ice_grid_i, import_slice, "inst_spec_humid_height2m" , "will provide", Qa)
   call dumpCICEInternal(ice_grid_i, import_slice, "xx_inst_air_density"      , "will provide", rhoa)
   call dumpCICEInternal(ice_grid_i, import_slice, "mean_down_sw_vis_dir_flx" , "will provide", swvdr)
   call dumpCICEInternal(ice_grid_i, import_slice, "mean_down_sw_vis_dif_flx" , "will provide", swvdf)
   call dumpCICEInternal(ice_grid_i, import_slice, "mean_down_sw_ir_dir_flx", "will provide", swidr)
   call dumpCICEInternal(ice_grid_i, import_slice, "mean_down_sw_ir_dif_flx", "will provide", swidf)
   call dumpCICEInternal(ice_grid_i, import_slice, "mean_down_lw_flx", "will provide", flw)
   call dumpCICEInternal(ice_grid_i, import_slice, "mean_prec_rate", "will provide", frain)
   call dumpCICEInternal(ice_grid_i, import_slice, "mean_fprec_rate", "will provide", fsnow)
   call dumpCICEInternal(ice_grid_i, import_slice, "ocn_current_zonal", "will provide", uocn)
   call dumpCICEInternal(ice_grid_i, import_slice, "ocn_current_merid", "will provide", vocn)
   call dumpCICEInternal(ice_grid_i, import_slice, "sea_surface_slope_zonal", "will provide", ss_tltx)
   call dumpCICEInternal(ice_grid_i, import_slice, "sea_surface_slope_merid", "will provide", ss_tlty)
   call dumpCICEInternal(ice_grid_i, import_slice, "sea_surface_salinity", "will provide", sss)
   call dumpCICEInternal(ice_grid_i, import_slice, "sea_surface_slope_zonal", "will provide", ss_tltx)
   call dumpCICEInternal(ice_grid_i, import_slice, "sea_surface_slope_merid", "will provide", ss_tlty)
   call dumpCICEInternal(ice_grid_i, import_slice, "sea_surface_temperature", "will provide", sst)
   call dumpCICEInternal(ice_grid_i, import_slice, "freezing_melting_potential", "will provide", frzmlt)
   call dumpCICEInternal(ice_grid_i, import_slice, "xx_inst_frz_mlt_potential", "will provide", frzmlt_init)
   call dumpCICEInternal(ice_grid_i, import_slice, "freezing_temp", "will provide", Tf)
   call dumpCICEInternal(ice_grid_i, import_slice, "mean_deep_ocean_down_heat_flx", "will provide", qdp)
   call dumpCICEInternal(ice_grid_i, import_slice, "mixed_layer_depth", "will provide", hmix)
   call dumpCICEInternal(ice_grid_i, import_slice, "xx_daice_da", "will provide", daice_da)

!--------- export fields from Sea Ice -------------

   call dumpCICEInternal(ice_grid_i, export_slice, "inst_ice_vis_dir_albedo"         , "will provide", alvdr)
   call dumpCICEInternal(ice_grid_i, export_slice, "inst_ice_ir_dir_albedo"          , "will provide", alidr)
   call dumpCICEInternal(ice_grid_i, export_slice, "inst_ice_vis_dif_albedo"         , "will provide", alvdf)
   call dumpCICEInternal(ice_grid_i, export_slice, "inst_ice_ir_dif_albedo"          , "will provide", alidf)
   call dumpCICEInternal(ice_grid_i, export_slice, "stress_on_air_ice_zonal"         , "will provide", strairxT)
   call dumpCICEInternal(ice_grid_i, export_slice, "stress_on_air_ice_merid"         , "will provide", strairyT)
   call dumpCICEInternal(ice_grid_i, export_slice, "stress_on_ocn_ice_zonal"         , "will provide", strocnxT)
   call dumpCICEInternal(ice_grid_i, export_slice, "stress_on_ocn_ice_merid"         , "will provide", strocnyT)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_sw_pen_to_ocn"              , "will provide", fswthru)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_net_sw_vis_dir_flx"         , "will provide", fswthruvdr)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_net_sw_vis_dif_flx"         , "will provide", fswthruvdf)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_net_sw_ir_dir_flx"          , "will provide", fswthruidr)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_net_sw_ir_dif_flx"          , "will provide", fswthruidf)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_up_lw_flx_ice"              , "will provide", flwout)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_sensi_heat_flx_atm_into_ice", "will provide", fsens)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_laten_heat_flx_atm_into_ice", "will provide", flat)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_evap_rate_atm_into_ice"     , "will provide", evap)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_fresh_water_to_ocean_rate"  , "will provide", fresh)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_salt_rate"                  , "will provide", fsalt)
   call dumpCICEInternal(ice_grid_i, export_slice, "net_heat_flx_to_ocn"             , "will provide", fhocn)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_ice_volume"                 , "will provide", vice)
   call dumpCICEInternal(ice_grid_i, export_slice, "mean_snow_volume"                , "will provide", vsno)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_inst_temp_height2m", "will provide", Tref)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_inst_spec_humid_height2m", "will provide", Qref)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_mean_albedo_vis_dir", "will provide", alvdr_ai)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_mean_albedo_nir_dir", "will provide", alidr_ai)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_mean_albedo_vis_dif", "will provide", alvdf_ai)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_mean_albedo_nir_dif", "will provide", alidf_ai)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_bare_ice_albedo", "will provide", albice)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_snow_albedo", "will provide", albsno)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_melt_pond_albedo", "will provide", albpnd)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_apeff_ai", "will provide", apeff_ai)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_mean_fresh_water_flx_to_ponds", "will provide", fpond)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_strairx_ocn", "will provide", strairx_ocn)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_strairy_ocn", "will provide", strairy_ocn)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_mean_sensi_heat_flx", "will provide", fsens_ocn)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_mean_laten_heat_flx", "will provide", flat_ocn)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_flwout_ocn", "will provide", flwout_ocn)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_evap_ocn", "will provide", evap_ocn)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_albedo_vis_dir", "will provide", alvdr_ocn)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_albedo_nir_dir", "will provide", alidr_ocn)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_albedo_vis_dif", "will provide", alvdf_ocn)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_albedo_nir_dif", "will provide", alidf_ocn)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_2m_atm_ref_temperature", "will provide", Tref_ocn)
   call dumpCICEInternal(ice_grid_i, export_slice, "xx_2m_atm_ref_spec_humidity", "will provide", Qref_ocn)
   if(profile_memory) call ESMF_VMLogMemInfo("Leaving CICE Model_ADVANCE: ")
  end subroutine 

  !> Called by NUOPC at the end of the run to clean up.  The cap does
  !! this simply by calling CICE_Finalize.
  !!
  !! @param gcomp the ESMF_GridComp object
  !! @param rc return code
  subroutine cice_model_finalize(gcomp, rc)

    ! input arguments
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)     :: clock
    type(ESMF_Time)                        :: currTime
    character(len=*),parameter  :: subname='(cice_cap:cice_model_finalize)'

    rc = ESMF_SUCCESS

    write(info,*) subname,' --- finalize called --- '
    call ESMF_LogWrite(info, ESMF_LOGMSG_INFO, rc=dbrc)

    call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call CICE_Finalize

    write(info,*) subname,' --- finalize completed --- '
    call ESMF_LogWrite(info, ESMF_LOGMSG_INFO, rc=dbrc)

  end subroutine cice_model_finalize

  !> Advertises a set of fields in an ESMF_State object by calling
  !! NUOPC_Advertise in a loop.
  !!
  !! @param state the ESMF_State object in which to advertise the fields
  !! @param nfields number of fields
  !! @param field_defs an array of fld_list_type listing the fields to advertise
  !! @param rc return code
  subroutine CICE_AdvertiseFields(state, nfields, field_defs, rc)

    type(ESMF_State), intent(inout)             :: state
    integer,intent(in)                          :: nfields
    type(fld_list_type), intent(inout)          :: field_defs(:)
    integer, intent(inout)                      :: rc

    integer                                     :: i
    character(len=*),parameter  :: subname='(cice_cap:CICE_AdvertiseFields)'

    rc = ESMF_SUCCESS

    do i = 1, nfields

      call ESMF_LogWrite('Advertise: '//trim(field_defs(i)%stdname), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call NUOPC_Advertise(state, &
        standardName=field_defs(i)%stdname, &
        name=field_defs(i)%shortname, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    enddo

  end subroutine CICE_AdvertiseFields

  !> Adds a set of fields to an ESMF_State object.  Each field is wrapped
  !! in an ESMF_Field object.  Memory is either allocated by ESMF or
  !! an existing CICE pointer is referenced.
  !!
  !! @param state the ESMF_State object to add fields to
  !! @param grid the ESMF_Grid object on which to define the fields
  !! @param nfields number of fields
  !! @param field_defs array of fld_list_type indicating the fields to add
  !! @param tag used to output to the log
  !! @param rc return code
  subroutine CICE_RealizeFields(state, grid, nfields, field_defs, tag, rc)

    type(ESMF_State), intent(inout)             :: state
    type(ESMF_Grid), intent(in)                 :: grid
    integer, intent(in)                         :: nfields
    type(fld_list_type), intent(inout)          :: field_defs(:)
    character(len=*), intent(in)                :: tag
    integer, intent(inout)                      :: rc

    integer                                     :: i
    type(ESMF_Field)                            :: field
    integer                                     :: npet, nx, ny, pet, elb(2), eub(2), clb(2), cub(2), tlb(2), tub(2)
    type(ESMF_VM)                               :: vm
    character(len=*),parameter  :: subname='(cice_cap:CICE_RealizeFields)'
 
    rc = ESMF_SUCCESS

      !call ESMF_VMGetCurrent(vm, rc=rc)
      !if (rc /= ESMF_SUCCESS) call ESMF_Finalize()

      !call ESMF_VMGet(vm, petcount=npet, localPet=pet, rc=rc)
      !if (rc /= ESMF_SUCCESS) call ESMF_Finalize()

      !call ESMF_GridGet(grid, exclusiveLBound=elb, exclusiveUBound=eub, &
      !                        computationalLBound=clb, computationalUBound=cub, &
      !                        totalLBound=tlb, totalUBound=tub, rc=rc)
      !if (rc /= ESMF_SUCCESS) call ESMF_Finalize()

      !write(info, *) pet, 'exc', elb, eub, 'comp', clb, cub, 'total', tlb, tub
      !call ESMF_LogWrite(subname // tag // " Grid "// info, &
      !  ESMF_LOGMSG_INFO, &
      !  line=__LINE__, &
      !  file=__FILE__, &
      !  rc=dbrc)

    do i = 1, nfields

      if (field_defs(i)%assoc) then
        write(info, *) subname, tag, ' Field ', field_defs(i)%shortname, ':', &
          lbound(field_defs(i)%farrayPtr,1), ubound(field_defs(i)%farrayPtr,1), &
          lbound(field_defs(i)%farrayPtr,2), ubound(field_defs(i)%farrayPtr,2), &
          lbound(field_defs(i)%farrayPtr,3), ubound(field_defs(i)%farrayPtr,3)
        call ESMF_LogWrite(info, ESMF_LOGMSG_INFO, rc=dbrc)
        field = ESMF_FieldCreate(grid=grid, &
          farray=field_defs(i)%farrayPtr, indexflag=ESMF_INDEX_DELOCAL, &
!          farray=field_defs(i)%farrayPtr, indexflag=ESMF_INDEX_GLOBAL, &
!          totalLWidth=(/1,1/), totalUWidth=(/1,1/),&
          ungriddedLBound=(/1/), ungriddedUBound=(/max_blocks/), &
          name=field_defs(i)%shortname, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      else
        field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, indexflag=ESMF_INDEX_DELOCAL, &
!          totalLWidth=(/1,1/), totalUWidth=(/1,1/),&
          ungriddedLBound=(/1/), ungriddedUBound=(/max_blocks/), &
          name=field_defs(i)%shortname, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif

      if (NUOPC_IsConnected(state, fieldName=field_defs(i)%shortname)) then
        call NUOPC_Realize(state, field=field, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call ESMF_LogWrite(subname // tag // " Field "// field_defs(i)%stdname // " is connected.", &
          ESMF_LOGMSG_INFO, &
          line=__LINE__, &
          file=__FILE__, &
          rc=dbrc)
!        call ESMF_FieldPrint(field=field, rc=rc)
!        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!          line=__LINE__, &
!          file=__FILE__)) &
!          return  ! bail out
      else
        call ESMF_LogWrite(subname // tag // " Field "// field_defs(i)%stdname // " is not connected.", &
          ESMF_LOGMSG_INFO, &
          line=__LINE__, &
          file=__FILE__, &
          rc=dbrc)
        ! TODO: Initialize the value in the pointer to 0 after proper restart is setup
        !if(associated(field_defs(i)%farrayPtr) ) field_defs(i)%farrayPtr = 0.0
        ! remove a not connected Field from State
        call ESMF_StateRemove(state, (/field_defs(i)%shortname/), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif

    enddo


  end subroutine CICE_RealizeFields

  !-----------------------------------------------------------------------------

  subroutine state_diagnose(State, string, rc)
    ! ----------------------------------------------
    ! Diagnose status of state
    ! ----------------------------------------------
    type(ESMF_State), intent(inout) :: State
    character(len=*), intent(in), optional :: string
    integer, intent(out), optional  :: rc

    ! local variables
    integer                     :: i,j,n
    integer                     :: fieldCount
    character(len=64) ,pointer  :: fieldNameList(:)
    character(len=64)           :: lstring
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:,:)
    integer                     :: lrc
    character(len=*),parameter  :: subname='(cice_cap:state_diagnose)'

    lstring = ''
    if (present(string)) then
       lstring = trim(string)
    endif

    call ESMF_StateGet(State, itemCount=fieldCount, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    allocate(fieldNameList(fieldCount))
    call ESMF_StateGet(State, itemNameList=fieldNameList, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    do n = 1, fieldCount
      call State_GetFldPtr(State, fieldNameList(n), dataPtr, rc=lrc)
      if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      write(tmpstr,'(A,3g14.7)') trim(subname)//' '//trim(lstring)//':'//trim(fieldNameList(n)), &
        minval(dataPtr),maxval(dataPtr),sum(dataPtr)
!      write(tmpstr,'(A)') trim(subname)//' '//trim(lstring)//':'//trim(fieldNameList(n))
      call ESMF_LogWrite(trim(tmpstr), ESMF_LOGMSG_INFO, rc=dbrc)
    enddo
    deallocate(fieldNameList)

    if (present(rc)) rc = lrc

  end subroutine state_diagnose

  !-----------------------------------------------------------------------------

  subroutine state_reset(State, value, rc)
    ! ----------------------------------------------
    ! Set all fields to value in State
    ! If value is not provided, reset to 0.0
    ! ----------------------------------------------
    type(ESMF_State), intent(inout) :: State
    real(ESMF_KIND_R8), intent(in), optional :: value
    integer, intent(out), optional  :: rc

    ! local variables
    integer                     :: i,j,k,n
    integer                     :: fieldCount
    character(len=64) ,pointer  :: fieldNameList(:)
    real(ESMF_KIND_R8)          :: lvalue
    real(ESMF_KIND_R8), pointer :: dataPtr(:,:,:)
    character(len=*),parameter :: subname='(cice_cap:state_reset)'

    if (present(rc)) rc = ESMF_SUCCESS

    lvalue = 0._ESMF_KIND_R8
    if (present(value)) then
      lvalue = value
    endif

    call ESMF_StateGet(State, itemCount=fieldCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    allocate(fieldNameList(fieldCount))
    call ESMF_StateGet(State, itemNameList=fieldNameList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    do n = 1, fieldCount
      call State_GetFldPtr(State, fieldNameList(n), dataPtr, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      do k=lbound(dataPtr,3),ubound(dataPtr,3)
      do j=lbound(dataPtr,2),ubound(dataPtr,2)
      do i=lbound(dataPtr,1),ubound(dataPtr,1)
         dataPtr(i,j,k) = lvalue
      enddo
      enddo
      enddo

    enddo
    deallocate(fieldNameList)

  end subroutine state_reset

  !-----------------------------------------------------------------------------

  !> Retrieve a pointer to a field's data array from inside an ESMF_State object.
  !!
  !! @param ST the ESMF_State object
  !! @param fldname name of the fields
  !! @param fldptr pointer to 3D array
  !! @param rc return code
  subroutine State_GetFldPtr(ST, fldname, fldptr, rc)
    type(ESMF_State), intent(in) :: ST
    character(len=*), intent(in) :: fldname
    real(ESMF_KIND_R8), pointer, intent(in) :: fldptr(:,:,:)
    integer, intent(out), optional :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    integer :: lrc
    character(len=*),parameter :: subname='(cice_cap:State_GetFldPtr)'

    call ESMF_StateGet(ST, itemName=trim(fldname), field=lfield, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (present(rc)) rc = lrc

  end subroutine State_GetFldPtr

  !-----------------------------------------------------------------------------
  logical function FieldBundle_FldChk(FB, fldname, rc)
    type(ESMF_FieldBundle), intent(in) :: FB
    character(len=*)      ,intent(in) :: fldname
    integer, intent(out), optional :: rc

    ! local variables
    integer :: lrc
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_FldChk)'

    if (present(rc)) rc = ESMF_SUCCESS

    FieldBundle_FldChk = .false.

    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), isPresent=isPresent, rc=lrc)
    if (present(rc)) rc = lrc
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    if (isPresent) then
       FieldBundle_FldChk = .true.
    endif

  end function FieldBundle_FldChk

  !-----------------------------------------------------------------------------

  subroutine FieldBundle_GetFldPtr(FB, fldname, fldptr, rc)
    type(ESMF_FieldBundle), intent(in) :: FB
    character(len=*)      , intent(in) :: fldname
    real(ESMF_KIND_R8), pointer, intent(in) :: fldptr(:,:)
    integer, intent(out), optional :: rc

    ! local variables
    type(ESMF_Field) :: lfield
    integer :: lrc
    character(len=*),parameter :: subname='(module_MEDIATOR:FieldBundle_GetFldPtr)'

    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldBundleGet(FB, fieldName=trim(fldname), field=lfield, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    call ESMF_FieldGet(lfield, farrayPtr=fldptr, rc=lrc)
    if (ESMF_LogFoundError(rcToCheck=lrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

    if (present(rc)) rc = lrc

  end subroutine FieldBundle_GetFldPtr

  !-----------------------------------------------------------------------------

  subroutine CICE_FieldsSetup
    character(len=*),parameter  :: subname='(cice_cap:CICE_FieldsSetup)'

!--------- import fields to Sea Ice -------------

! tcraig, don't point directly into cice data YET (last field is optional in interface)
! instead, create space for the field when it's "realized".
    call fld_list_add(fldsToIce_num, fldsToIce, "inst_height_lowest"       , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "inst_temp_height_lowest"       , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "inst_spec_humid_height_lowest" , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "inst_zonal_wind_height_lowest" , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "inst_merid_wind_height_lowest" , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "inst_pres_height_lowest"       , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "mean_down_lw_flx"         , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "mean_down_sw_vis_dir_flx" , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "mean_down_sw_vis_dif_flx" , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "mean_down_sw_ir_dir_flx"  , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "mean_down_sw_ir_dif_flx"  , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "mean_prec_rate"           , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "mean_fprec_rate"          , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "sea_surface_temperature"  , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "s_surf"                   , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "sea_lev"                  , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "sea_surface_slope_zonal"  , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "sea_surface_slope_merid"  , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "ocn_current_zonal"        , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "ocn_current_merid"        , "will provide")
!    call fld_list_add(fldsToIce_num, fldsToIce, "ocn_current_idir"         , "will provide")
!    call fld_list_add(fldsToIce_num, fldsToIce, "ocn_current_jdir"         , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "freezing_melting_potential", "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "mixed_layer_depth"        , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "mean_zonal_moment_flx", "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "mean_merid_moment_flx", "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "inst_surface_height"  , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "inst_temp_height2m"  , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "inst_spec_humid_height2m"  , "will provide")
    call fld_list_add(fldsToIce_num, fldsToIce, "air_density_height_lowest"  , "will provide")

!   call fld_list_add(fldsToIce_num, fldsToIce, "inst_zonal_wind_height10m", "will provide", strax)
!   call fld_list_add(fldsToIce_num, fldsToIce, "inst_merid_wind_height10m", "will provide", stray)
!   call fld_list_add(fldsToIce_num, fldsToIce, "inst_pres_height_surface" , "will provide", zlvl)
!   call fld_list_add(fldsToIce_num, fldsToIce, "xx_pot_air_temp"          , "will provide", potT)
!   call fld_list_add(fldsToIce_num, fldsToIce, "inst_temp_height2m"       , "will provide", Tair)
!   call fld_list_add(fldsToIce_num, fldsToIce, "inst_spec_humid_height2m" , "will provide", Qa)
!   call fld_list_add(fldsToIce_num, fldsToIce, "xx_inst_air_density"      , "will provide", rhoa)
!   call fld_list_add(fldsToIce_num, fldsToIce, "mean_down_sw_vis_dir_flx" , "will provide", swvdr)
!   call fld_list_add(fldsToIce_num, fldsToIce, "mean_down_sw_vis_dif_flx" , "will provide", swvdf)
!   call fld_list_add(fldsToIce_num, fldsToIce, "mean_down_sw_ir_dir_flx", "will provide", swidr)
!   call fld_list_add(fldsToIce_num, fldsToIce, "mean_down_sw_ir_dif_flx", "will provide", swidf)
!   call fld_list_add(fldsToIce_num, fldsToIce, "mean_down_lw_flx", "will provide", flw)
!   call fld_list_add(fldsToIce_num, fldsToIce, "mean_prec_rate", "will provide", frain)
!   call fld_list_add(fldsToIce_num, fldsToIce, "xx_mean_fprec_rate", "will provide", frain)
!   call fld_list_add(fldsToIce_num, fldsToIce, "xx_faero_atm", "will provide", faero_atm)
!   call fld_list_add(fldsToIce_num, fldsToIce, "ocn_current_zonal", "will provide", uocn)
!   call fld_list_add(fldsToIce_num, fldsToIce, "ocn_current_merid", "will provide", vocn)
!   call fld_list_add(fldsToIce_num, fldsToIce, "sea_surface_slope_zonal", "will provide", ss_tltx)
!   call fld_list_add(fldsToIce_num, fldsToIce, "sea_surface_slope_merid", "will provide", ss_tlty)
!   call fld_list_add(fldsToIce_num, fldsToIce, "s_surf", "will provide", sss)
!   call fld_list_add(fldsToIce_num, fldsToIce, "sea_surface_temperature", "will provide", sst)
!   call fld_list_add(fldsToIce_num, fldsToIce, "freezing_melting_potential", "will provide", frzmlt)
!   call fld_list_add(fldsToIce_num, fldsToIce, "xx_inst_frz_mlt_potential", "will provide", frzmlt_init)
!   call fld_list_add(fldsToIce_num, fldsToIce, "freezing_temp", "will provide", Tf)
!   call fld_list_add(fldsToIce_num, fldsToIce, "mean_deep_ocean_down_heat_flx", "will provide", qdp)
!   call fld_list_add(fldsToIce_num, fldsToIce, "mixed_layer_depth", "will provide", hmix)
!   call fld_list_add(fldsToIce_num, fldsToIce, "xx_daice_da", "will provide", daice_da)

!--------- export fields from Sea Ice -------------

!tcx    call fld_list_add(fldsFrIce_num, fldsFrIce, "sea_ice_temperature"             , "will provide", icetemp_cpl)
!tcx    call fld_list_add(fldsFrIce_num, fldsFrIce, "inst_ice_vis_dir_albedo"         , "will provide", alvdr)
!tcx    call fld_list_add(fldsFrIce_num, fldsFrIce, "inst_ice_ir_dir_albedo"          , "will provide", alidr)
!tcx    call fld_list_add(fldsFrIce_num, fldsFrIce, "inst_ice_vis_dif_albedo"         , "will provide", alvdf)
!tcx    call fld_list_add(fldsFrIce_num, fldsFrIce, "inst_ice_ir_dif_albedo"          , "will provide", alidf)
!tcx    call fld_list_add(fldsFrIce_num, fldsFrIce, "ice_fraction"                    , "will provide", aice_cpl)
!    call fld_list_add(fldsFrIce_num, fldsFrIce, "stress_on_air_ice_zonal"         , "will provide", strairxT)
!    call fld_list_add(fldsFrIce_num, fldsFrIce, "stress_on_air_ice_merid"         , "will provide", strairyT)
!    call fld_list_add(fldsFrIce_num, fldsFrIce, "stress_on_ocn_ice_zonal"         , "will provide", strocnxT)
!    call fld_list_add(fldsFrIce_num, fldsFrIce, "stress_on_ocn_ice_merid"         , "will provide", strocnyT)
!    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_sw_pen_to_ocn"              , "will provide", fswthru)
!    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_up_lw_flx_ice"              , "will provide", flwout)
!    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_sensi_heat_flx_atm_into_ice", "will provide", fsens)
!    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_laten_heat_flx_atm_into_ice", "will provide", flat)
!tcx    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_evap_rate_atm_into_ice"     , "will provide", evap)
    call fld_list_add(fldsFrIce_num, fldsFrIce, "sea_ice_temperature"             , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "inst_ice_vis_dir_albedo"         , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "inst_ice_ir_dir_albedo"          , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "inst_ice_vis_dif_albedo"         , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "inst_ice_ir_dif_albedo"          , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "ice_mask"                        , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "ice_fraction"                    , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "stress_on_air_ice_zonal"         , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "stress_on_air_ice_merid"         , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "stress_on_ocn_ice_zonal"         , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "stress_on_ocn_ice_merid"         , "will provide")
!    call fld_list_add(fldsFrIce_num, fldsFrIce, "stress_on_ocn_ice_idir"          , "will provide")
!    call fld_list_add(fldsFrIce_num, fldsFrIce, "stress_on_ocn_ice_jdir"          , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_sw_pen_to_ocn"              , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_net_sw_vis_dir_flx"         , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_net_sw_vis_dif_flx"         , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_net_sw_ir_dir_flx"          , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_net_sw_ir_dif_flx"          , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_up_lw_flx_ice"              , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_sensi_heat_flx_atm_into_ice", "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_laten_heat_flx_atm_into_ice", "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_evap_rate_atm_into_ice"     , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_fresh_water_to_ocean_rate"  , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_salt_rate"                  , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "net_heat_flx_to_ocn"             , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_ice_volume"                 , "will provide")
    call fld_list_add(fldsFrIce_num, fldsFrIce, "mean_snow_volume"                , "will provide")

!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_inst_temp_height2m", "will provide", Tref)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_inst_spec_humid_height2m", "will provide", Qref)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_mean_albedo_vis_dir", "will provide", alvdr_ai)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_mean_albedo_nir_dir", "will provide", alidr_ai)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_mean_albedo_vis_dif", "will provide", alvdf_ai)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_mean_albedo_nir_dif", "will provide", alidf_ai)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_bare_ice_albedo", "will provide", albice)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_snow_albedo", "will provide", albsno)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_melt_pond_albedo", "will provide", albpnd)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_apeff_ai", "will provide", apeff_ai)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_mean_fresh_water_flx_to_ponds", "will provide", fpond)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_faero_ocn", "will provide", faero_ocn)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_strairx_ocn", "will provide", strairx_ocn)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_strairy_ocn", "will provide", strairy_ocn)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_mean_sensi_heat_flx", "will provide", fsens_ocn)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_mean_laten_heat_flx", "will provide", flat_ocn)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_flwout_ocn", "will provide", flwout_ocn)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_evap_ocn", "will provide", evap_ocn)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_albedo_vis_dir", "will provide", alvdr_ocn)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_albedo_nir_dir", "will provide", alidr_ocn)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_albedo_vis_dif", "will provide", alvdf_ocn)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_albedo_nir_dif", "will provide", alidf_ocn)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_2m_atm_ref_temperature", "will provide", Tref_ocn)
!   call fld_list_add(fldsFrIce_num, fldsFrIce, "xx_2m_atm_ref_spec_humidity", "will provide", Qref_ocn)


  end subroutine CICE_FieldsSetup

  !-----------------------------------------------------------------------------

  subroutine fld_list_add(num, fldlist, stdname, transferOffer, data, shortname)
    ! ----------------------------------------------
    ! Set up a list of field information
    ! ----------------------------------------------
    integer,             intent(inout)  :: num
    type(fld_list_type), intent(inout)  :: fldlist(:)
    character(len=*),    intent(in)     :: stdname
    character(len=*),    intent(in)     :: transferOffer
    real(ESMF_KIND_R8), dimension(:,:,:), optional, target :: data
    character(len=*),    intent(in),optional :: shortname

    ! local variables
    integer :: rc
    character(len=*), parameter :: subname='(cice_cap:fld_list_add)'

    ! fill in the new entry

    num = num + 1
    if (num > fldsMax) then
      call ESMF_LogWrite(trim(subname)//": ERROR num gt fldsMax "//trim(stdname), &
        ESMF_LOGMSG_ERROR, line=__LINE__, file=__FILE__, rc=dbrc)
      return
    endif

    fldlist(num)%stdname        = trim(stdname)
    if (present(shortname)) then
       fldlist(num)%shortname   = trim(shortname)
    else
       fldlist(num)%shortname   = trim(stdname)
    endif
    fldlist(num)%transferOffer  = trim(transferOffer)
    if (present(data)) then
      fldlist(num)%assoc        = .true.
      fldlist(num)%farrayPtr    => data
    else
      fldlist(num)%assoc        = .false.
    endif

  end subroutine fld_list_add

  !> Writes out a diagnostic file containing field data named
  !! field_ice_internal_<fldname>.nc.
  !!
  !! @param grid the ESMF_Grid on describing the field's grid
  !! @param slice time spice number
  !! @param stdname standard name of the field
  !! @param nop ignored for now
  !! @param farray array of data to write
  subroutine dumpCICEInternal(grid, slice, stdname, nop, farray)

    type(ESMF_Grid)          :: grid
    integer, intent(in)      :: slice
    character(len=*)         :: stdname
    character(len=*)         :: nop
    real(ESMF_KIND_R8), dimension(:,:,:), target :: farray

    type(ESMF_Field)         :: field
    real(ESMF_KIND_R8), dimension(:,:), pointer  :: f2d
    integer                  :: rc

    if(.not. write_diagnostics) return ! remove this line to debug field connection

    field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, &
      indexflag=ESMF_INDEX_DELOCAL, &
      name=stdname, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldGet(field, farrayPtr=f2d, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    f2d(:,:) = farray(:,:,1)

    call ESMF_FieldWrite(field, fileName='field_ice_internal_'//trim(stdname)//'.nc', &
      timeslice=slice, rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldDestroy(field, noGarbage=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine

  !-----------------------------------------------------------------------------
end module cice_cap_mod
