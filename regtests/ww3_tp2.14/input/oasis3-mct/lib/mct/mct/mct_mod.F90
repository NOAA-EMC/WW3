module mct_mod

! !USES:
   use m_MCTWorld           ,only: mct_world_init         => init

   use m_AttrVect           ,only: mct_aVect              => AttrVect
   use m_AttrVect           ,only: mct_aVect_init         => init
   use m_AttrVect           ,only: mct_aVect_clean        => clean
   use m_AttrVect           ,only: mct_aVect_zero         => zero
   use m_AttrVect           ,only: mct_aVect_lsize        => lsize
   use m_AttrVect           ,only: mct_aVect_indexIA      => indexIA
   use m_AttrVect           ,only: mct_aVect_indexRA      => indexRA
   use m_AttrVect           ,only: mct_aVect_importRattr  => importRattr
   use m_AttrVect           ,only: mct_aVect_exportRattr  => exportRattr
   use m_AttrVect           ,only: mct_aVect_getIList     => getIList
   use m_AttrVect           ,only: mct_aVect_getRList     => getRList
   use m_AttrVect           ,only: mct_aVect_exportIList2c=> exportIListToChar
   use m_AttrVect           ,only: mct_aVect_exportRList2c=> exportRListToChar
   use m_AttrVect           ,only: mct_aVect_nIAttr       => nIAttr
   use m_AttrVect           ,only: mct_aVect_nRAttr       => nRAttr
   use m_AttrVect           ,only: mct_aVect_copy         => Copy
   use m_AttrVect           ,only: mct_aVect_permute      => Permute
   use m_AttrVect           ,only: mct_aVect_unpermute    => Unpermute
   use m_AttrVectComms      ,only: mct_aVect_scatter      => scatter
   use m_AttrVectComms      ,only: mct_aVect_gather       => gather 
   use m_AttrVectComms      ,only: mct_aVect_bcast        => bcast  

   use m_Accumulator        ,only: mct_accum              => Accumulator
   use m_Accumulator        ,only: mct_accum_init         => init
   use m_Accumulator        ,only: mct_accum_zero         => zero
   use m_Accumulator        ,only: mct_accum_accumulate   => accumulate
!   use m_Accumulator        ,only: mct_accum_average      => average

   use m_GeneralGrid        ,only: mct_gGrid              => GeneralGrid
   use m_GeneralGrid        ,only: mct_gGrid_init         => init
   use m_GeneralGrid        ,only: mct_gGrid_clean        => clean
   use m_GeneralGrid        ,only: mct_gGrid_dims         => dims
   use m_GeneralGrid        ,only: mct_gGrid_lsize        => lsize
   use m_GeneralGrid        ,only: mct_ggrid_indexIA      => indexIA
   use m_GeneralGrid        ,only: mct_gGrid_indexRA      => indexRA
   use m_GeneralGrid        ,only: mct_gGrid_exportRattr  => exportRattr
   use m_GeneralGrid        ,only: mct_gGrid_importRattr  => importRattr
   use m_GeneralGrid        ,only: mct_gGrid_exportIattr  => exportIattr
   use m_GeneralGrid        ,only: mct_gGrid_importIattr  => importIattr
   use m_GeneralGrid        ,only: mct_gGrid_permute      => permute
   use m_GeneralGridComms   ,only: mct_gGrid_scatter      => scatter
   use m_GeneralGridComms   ,only: mct_gGrid_gather       => gather 
   use m_GeneralGridComms   ,only: mct_gGrid_bcast        => bcast  

   use m_Transfer           ,only: mct_send               => Send
   use m_Transfer           ,only: mct_isend              => iSend
   use m_Transfer           ,only: mct_waitsend           => WaitSend
   use m_Transfer           ,only: mct_recv               => Recv
   use m_Transfer           ,only: mct_irecv              => iRecv
   use m_Transfer           ,only: mct_waitrecv           => WaitRecv
      
   use m_GlobalSegMap       ,only: mct_gsMap              => GlobalSegMap
   use m_GlobalSegMap       ,only: mct_gsMap_init         => init
   use m_GlobalSegMap       ,only: mct_gsMap_clean        => clean
   use m_GlobalSegMap       ,only: mct_gsMap_lsize        => lsize
   use m_GlobalSegMap       ,only: mct_gsMap_gsize        => gsize
   use m_GlobalSegMap       ,only: mct_gsMap_gstorage     => GlobalStorage
   use m_GlobalSegMap       ,only: mct_gsMap_ngseg        => ngseg
   use m_GlobalSegMap       ,only: mct_gsMap_nlseg        => nlseg
   use m_GlobalSegMap       ,only: mct_gsMap_maxnlseg     => max_nlseg
   use m_GlobalSegMap       ,only: mct_gsMap_activepes    => active_pes
   use m_GlobalSegMap       ,only: mct_gsMap_copy         => copy
   use m_GlobalSegMap       ,only: mct_gsMap_increasing   => increasing
   use m_GlobalSegMap       ,only: mct_gsMap_orderedPoints=> OrderedPoints
   use m_GlobalSegMapComms  ,only: mct_gsMap_bcast        => bcast 

   use m_Rearranger         ,only: mct_rearr              => Rearranger
   use m_Rearranger         ,only: mct_rearr_init         => init
   use m_Rearranger         ,only: mct_rearr_clean        => clean
   use m_Rearranger         ,only: mct_rearr_print        => print
   use m_Rearranger         ,only: mct_rearr_rearrange    => rearrange

   use m_Router             ,only: mct_router             => Router
   use m_Router             ,only: mct_router_init        => init

   use m_SparseMatrixToMaps ,only: mct_sMat_2XgsMap       => SparseMatrixToXGlobalSegMap
   use m_SparseMatrixToMaps ,only: mct_sMat_2YgsMap       => SparseMatrixToYGlobalSegMap
   use m_SparseMatrix       ,only: mct_sMat               => SparseMatrix
   use m_SparseMatrix       ,only: mct_sMat_Init          => init
   use m_SparseMatrix       ,only: mct_sMat_Vecinit       => vecinit
   use m_SparseMatrix       ,only: mct_sMat_Clean         => clean
   use m_SparseMatrix       ,only: mct_sMat_indexIA       => indexIA
   use m_SparseMatrix       ,only: mct_sMat_indexRA       => indexRA
   use m_SparseMatrix       ,only: mct_sMat_lsize         => lsize
   use m_SparseMatrix       ,only: mct_sMat_nrows         => nRows
   use m_SparseMatrix       ,only: mct_sMat_ncols         => nCols
   use m_SparseMatrix       ,only: mct_sMat_SortPermute   => SortPermute
   use m_SparseMatrix       ,only: mct_sMat_GNumEl        => GlobalNumElements
   use m_SparseMatrix       ,only: mct_sMat_ImpGRowI      => ImportGlobalRowIndices
   use m_SparseMatrix       ,only: mct_sMat_ImpGColI      => ImportGlobalColumnIndices
   use m_SparseMatrix       ,only: mct_sMat_ImpLRowI      => ImportLocalRowIndices
   use m_SparseMatrix       ,only: mct_sMat_ImpLColI      => ImportLocalColumnIndices
   use m_SparseMatrix       ,only: mct_sMat_ImpMatrix     => ImportMatrixElements
   use m_SparseMatrix       ,only: mct_sMat_ExpGRowI      => ExportGlobalRowIndices
   use m_SparseMatrix       ,only: mct_sMat_ExpGColI      => ExportGlobalColumnIndices
   use m_SparseMatrix       ,only: mct_sMat_ExpLRowI      => ExportLocalRowIndices
   use m_SparseMatrix       ,only: mct_sMat_ExpLColI      => ExportLocalColumnIndices
   use m_SparseMatrix       ,only: mct_sMat_ExpMatrix     => ExportMatrixElements
   use m_SparseMatrixComms  ,only: mct_sMat_ScatterByRow  => ScatterByRow
   use m_SparseMatrixComms  ,only: mct_sMat_ScatterByCol  => ScatterByColumn
   use m_SparseMatrixPlus   ,only: mct_sMatP              => SparseMatrixPlus
   use m_SparseMatrixPlus   ,only: mct_sMatP_Init         => init
   use m_SparseMatrixPlus   ,only: mct_sMatP_Vecinit      => vecinit
   use m_SparseMatrixPlus   ,only: mct_sMatP_clean        => clean
   use m_MatAttrVectMul     ,only: mct_sMat_avMult        => sMatAvMult
   use m_GlobalToLocal      ,only: mct_sMat_g2lMat        => GlobalToLocalMatrix

   use m_List               ,only: mct_list               => list     
   use m_List               ,only: mct_list_init          => init
   use m_List               ,only: mct_list_get           => get 
   use m_List               ,only: mct_list_nitem         => nitem 
   use m_List               ,only: mct_list_clean         => clean
   use m_string             ,only: mct_string             => string 
   use m_string             ,only: mct_string_clean       => clean
   use m_string             ,only: mct_string_toChar      => toChar 
   use m_die                ,only: mct_perr_die           => mp_perr_die
   use m_die                ,only: mct_die                => die
   use m_inpak90


   use m_Permuter           ,only: mct_permute            => Permute

   use m_MergeSorts         ,only: mct_indexset           => IndexSet
   use m_MergeSorts         ,only: mct_indexsort          => IndexSort

   implicit none

!===============================================================================

end module mct_mod
