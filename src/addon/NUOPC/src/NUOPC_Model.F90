! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/src/NUOPC_Model.F90"
!==============================================================================

! access platform dependent macros
#include "ESMF_Conf.inc"

module NUOPC_Model

  !-----------------------------------------------------------------------------
  ! Generic Model Component
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_ModelBase, only: &
    SetVM, &
    ModelBase_routine_SS            => SetServices, &
    routine_Run, &
    routine_Nop, &
    label_Advertise, &
    label_ModifyAdvertised, &
    label_RealizeProvided, &
    label_AcceptTransfer, &
    label_RealizeAccepted, &
    label_SetClock, &
    label_DataInitialize, &
    label_Advance, &
    label_AdvanceClock, &
    label_CheckImport, &
    label_SetRunClock, &
    label_TimestampExport, &
    label_Finalize, &
    type_InternalStateStruct, type_InternalState, label_InternalState, &
    NUOPC_ModelBaseGet

  implicit none

  private

  public &
    SetVM, &
    SetServices, &
    routine_Run

  public &
    label_Advertise, &
    label_ModifyAdvertised, &
    label_RealizeProvided, &
    label_AcceptTransfer, &
    label_RealizeAccepted, &
    label_SetClock, &
    label_DataInitialize, &
    label_Advance, &
    label_AdvanceClock, &
    label_CheckImport, &
    label_SetRunClock, &
    label_TimestampExport, &
    label_Finalize

  ! Generic methods
  public NUOPC_ModelGet
  public NUOPC_ModelTagAdd
  public NUOPC_ModelTagPrint

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc

    ! local variables
    character(ESMF_MAXSTR):: name

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) return  ! bail out

    ! Derive from ModelBase
    call NUOPC_CompDerive(gcomp, ModelBase_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! Specialize Run -> timestamp export Fields
    call NUOPC_CompSpecialize(gcomp, specLabel=label_TimestampExport, &
      specRoutine=TimestampExport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! Set IPDvX attribute
    call NUOPC_CompAttributeSet(gcomp, name="IPDvX", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine TimestampExport(gcomp, rc)
    type(ESMF_GridComp)   :: gcomp
    integer, intent(out)  :: rc

    ! local variables
    type(ESMF_Clock)          :: clock
    character(ESMF_MAXSTR)    :: name
    type(type_InternalState)  :: is

    rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(gcomp, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! get to the clock
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME)) &
      return  ! bail out

    ! query Component for the internal State
    nullify(is%wrap)
#ifdef ESMF_NO_F2018ASSUMEDTYPE
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
#else
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc=rc)
#endif
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

    ! update timestamp on export Fields
    if (associated(is%wrap%cachedExportFieldList)) then
      call NUOPC_SetTimestamp(is%wrap%cachedExportFieldList, clock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//FILENAME)) &
        return  ! bail out
    endif

  end subroutine

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ModelGet - Get info from a Model
!
! !INTERFACE:
  subroutine NUOPC_ModelGet(model, driverClock, modelClock, &
    importState, exportState, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                        :: model
    type(ESMF_Clock),    intent(out), optional :: driverClock
    type(ESMF_Clock),    intent(out), optional :: modelClock
    type(ESMF_State),    intent(out), optional :: importState
    type(ESMF_State),    intent(out), optional :: exportState
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Access Model information.
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name

    if (present(rc)) rc = ESMF_SUCCESS

    ! query the component for info
    call NUOPC_CompGet(model, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out

    ! query ModeBase
    call NUOPC_ModelBaseGet(model, driverClock=driverClock, clock=modelClock, &
      importState=importState, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ModelTagAdd - Add tag to NUOPC Model
! !INTERFACE:
  ! Private name; call using NUOPC_ModelTagAdd()
  subroutine NUOPC_ModelTagAdd(model, tag, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: model
    character(len=*), intent(in)            :: tag
    integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
! Add tag to NUOPC Model
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(ESMF_State)                :: exportState
    type(ESMF_Info)                 :: exportInfo

    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_CompGet(model, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    call NUOPC_ModelGet(model, exportState=exportState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_InfoGetFromHost(exportState, info=exportInfo, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_InfoSet(exportInfo, "/NUOPC/__tags__/"//trim(name), tag, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
!BOP
! !IROUTINE: NUOPC_ModelTagPrint - Print tags for NUOPC Model
! !INTERFACE:
  ! Private name; call using NUOPC_ModelTagPrint()
  subroutine NUOPC_ModelTagPrint(model, rc)
! !ARGUMENTS:
    type(ESMF_GridComp)                     :: model
    integer,          intent(out), optional :: rc
!
! !DESCRIPTION:
! Print tags for NUOPC Model
!EOP
  !-----------------------------------------------------------------------------
    ! local variables
    integer                         :: localrc
    character(ESMF_MAXSTR)          :: name
    type(ESMF_State)                :: importState
    type(ESMF_Info)                 :: importInfo
    type(ESMF_Info)                 :: tagInfo

    if (present(rc)) rc = ESMF_SUCCESS

    call NUOPC_CompGet(model, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) return  ! bail out
    call NUOPC_ModelGet(model, importState=importState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_InfoGetFromHost(importState, info=importInfo, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    tagInfo = ESMF_InfoCreate(importInfo, key="/NUOPC/__tags__", rc=localrc)
    call ESMF_InfoPrint(tagInfo, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out
    call ESMF_InfoDestroy(tagInfo, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//FILENAME, rcToReturn=rc)) &
      return  ! bail out

  end subroutine
  !-----------------------------------------------------------------------------

end module
