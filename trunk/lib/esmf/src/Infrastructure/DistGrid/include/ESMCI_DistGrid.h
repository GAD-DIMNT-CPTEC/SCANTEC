// $Id: ESMCI_DistGrid.h,v 1.22.2.1 2010/02/05 19:55:11 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMCI_DistGrid_H
#define ESMCI_DistGrid_H

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::DistGrid - DistGrid
//
// !DESCRIPTION:
//
// The code in this file defines the C++ {\tt DistGrid} members and method
// signatures (prototypes).  The companion file {\tt ESMC\_DistGrid.C}
// contains the full code (bodies) for the {\tt DistGrid} methods.
//
//EOPI
//-------------------------------------------------------------------------

#include "ESMC_Base.h"      // Base is superclass to DistGrid
#include "ESMCI_VM.h"
#include "ESMCI_DELayout.h"

//-------------------------------------------------------------------------

namespace ESMCI {

  // constants and enums

  enum DecompFlag {DECOMP_DEFAULT=1, DECOMP_HOMOGEN,
    DECOMP_RESTFIRST, DECOMP_RESTLAST, DECOMP_CYCLIC};

  // classes

  class DistGrid;

  // class definition
  class DistGrid : public ESMC_Base {    // inherits from ESMC_Base class

   private:
    int dimCount;                 // rank of DistGrid
    int patchCount;               // number of patches in DistGrid
    int *minIndexPDimPPatch;      // lower corner indices [dimCount*patchCount]
    int *maxIndexPDimPPatch;      // upper corner indices [dimCount*patchCount]
    int *elementCountPPatch;      // number of elements [patchCount]
    int *minIndexPDimPDe;         // lower corner indices [dimCount*deCount]
    int *maxIndexPDimPDe;         // upper corner indices [dimCount*deCount]
    int *elementCountPDe;         // number of elements [deCount]
    int *patchListPDe;            // patch indices [deCount]
    int *contigFlagPDimPDe;       // flag contiguous indices [dimCount*deCount]
    int *indexCountPDimPDe;       // number of indices [dimCount*deCount]
    int **indexListPDimPLocalDe;  // local DEs' indices [dimCount*localDeCount]
                                  // [indexCountPDimPDe(localDe,dim)]
    int connectionCount;          // number of elements in connection list
    int **connectionList;         // list of connection elements
    int ***arbSeqIndexListPCollPLocalDe;// local arb sequence indices
                                  // [diffCollocationCount][localDeCount]
                                  // [elementCountPCollPLocalDe(localDe)]
    int *collocationPDim;         // collocation [dimCount]
    int diffCollocationCount;     // number different seqIndex collocations
    int *collocationTable;        // collocation in packed format [dimCount]
    int **elementCountPCollPLocalDe; // number of elements 
                                  // [diffCollocationCount][localDeCount]
    int *regDecomp;               // regular decomposition descriptor [dimCount]
    // lower level object references
    DELayout *delayout;
    bool delayoutCreator;
    VM *vm;    
    int localDeCountAux;  // auxiliary variable for garbage collection
                          // TODO: remove this variable again once
                          // TODO: reference counting scheme is implemented
                          // TODO: and DELayout cannot be pulled from under
                          // TODO: DistGrid and Array until they are destroyed.
        
   public:
    // native constructor and destructor
    DistGrid(){}
    DistGrid(int baseID):ESMC_Base(baseID){}// prevent baseID counter increment
    ~DistGrid(){destruct(false);}
    
   private:
    // construct() and destruct()
    int construct(int dimCount, int patchCount, int *dePatchList,
      int *minIndex, int *maxIndex, int *minIndexPDimPDe, int *maxIndexPDimPDe,
      int *contigFlagPDimPDe, int *indexCountPDimPDe, int **indexList,
      int *regDecompArg, InterfaceInt *connectionList,
      DELayout *delayout, bool delayoutCreator, VM *vm);
    int destruct(bool followCreator=true);
   public:
    // create() and destroy()
    static DistGrid *create(DistGrid const *dg,
      InterfaceInt *firstExtra, InterfaceInt *lastExtra, 
      ESMC_IndexFlag *indexflag, int *rc=NULL);
    static DistGrid *create(InterfaceInt *minIndex,
      InterfaceInt *maxIndex, InterfaceInt *regDecomp, 
      DecompFlag *decompflag, int decompflagCount,
      InterfaceInt *regDecompFirstExtra, InterfaceInt *regDecompLastExtra, 
      InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
      InterfaceInt *connectionList,
      DELayout *delayout=NULL, VM *vm=NULL, int *rc=NULL);
    static DistGrid *create(InterfaceInt *minIndex,
      InterfaceInt *maxIndex, InterfaceInt *deBlockList, 
      InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
      InterfaceInt *connectionList,
      DELayout *delayout=NULL, VM *vm=NULL, int *rc=NULL);
    static DistGrid *create(InterfaceInt *minIndex,
      InterfaceInt *maxIndex, InterfaceInt *regDecomp, 
      DecompFlag *decompflag, int decompflagCount,
      InterfaceInt *regDecompFirstExtra, InterfaceInt *regDecompLastExtra, 
      InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
      InterfaceInt *connectionList,
      int fastAxis, VM *vm=NULL, int *rc=NULL);
    static DistGrid *create(InterfaceInt *minIndex,
      InterfaceInt *maxIndex, InterfaceInt *regDecomp, 
      DecompFlag *decompflag, int decompflagCount1, int decompflagCount2,
      InterfaceInt *regDecompFirstExtra, InterfaceInt *regDecompLastExtra, 
      InterfaceInt *deLabelList, ESMC_IndexFlag *indexflag, 
      InterfaceInt *connectionList,
      DELayout *delayout=NULL, VM *vm=NULL, int *rc=NULL);
    static int destroy(DistGrid **distgrid);
    // is()
    bool isLocalDeOnEdgeL(int localDe, int dim, int *rc) const;
    bool isLocalDeOnEdgeU(int localDe, int dim, int *rc) const;
    // get() and set()
    int getDimCount()                   const {return dimCount;}
    int getPatchCount()                 const {return patchCount;}
    int getDiffCollocationCount()   const {return diffCollocationCount;}
    const int *getMinIndexPDimPPatch()  const {return minIndexPDimPPatch;}
    const int *getMinIndexPDimPPatch(int patch, int *rc) const;
    const int *getMaxIndexPDimPPatch()  const {return maxIndexPDimPPatch;}
    const int *getMaxIndexPDimPPatch(int patch, int *rc) const;
    const int *getElementCountPPatch()  const {return elementCountPPatch;}
    const int *getMinIndexPDimPDe()     const {return minIndexPDimPDe;}
    const int *getMinIndexPDimPDe(int de, int *rc) const;
    const int *getMaxIndexPDimPDe()     const {return maxIndexPDimPDe;}
    const int *getMaxIndexPDimPDe(int de, int *rc) const;
    const int *getElementCountPDe()      const {return elementCountPDe;}
    int getElementCountPDe(int de, int *rc) const;
    const int *getPatchListPDe()        const {return patchListPDe;}
    const int *getContigFlagPDimPDe()   const {return contigFlagPDimPDe;}
    int getContigFlagPDimPDe(int de, int dim, int *rc) const;
    const int *getIndexCountPDimPDe()   const {return indexCountPDimPDe;}
    const int *getIndexListPDimPLocalDe(int localDe, int dim, int *rc=NULL)
      const;
    const int *getCollocationPDim() const {return collocationPDim;}
    const int *getCollocationTable() const {return collocationTable;}
    DELayout *getDELayout()         const {return delayout;}
    const int *getRegDecomp()       const {return regDecomp;}
    int getSequenceIndexLocalDe(int localDe, const int *index, int *rc=NULL)
      const;
    int getSequenceIndexPatchRelative(int patch, const int *index, int depth,
      int *rc=NULL)const;
    int getSequenceIndexPatch(int patch, const int *index, int depth,
      int *rc=NULL)const;
    int *const*getElementCountPCollPLocalDe()
      const {return elementCountPCollPLocalDe;}
    const int *getArbSeqIndexListPLocalDe(int localDe, int collocation,
      int *rc=NULL) const;
    int setArbSeqIndex(InterfaceInt *arbSeqIndex, int localDe, int collocation);
    int setCollocationPDim(InterfaceInt *collocationPDim);
    // fill()
    int fillIndexListPDimPDe(int *indexList, int de, int dim,
      VMK::commhandle **commh, int rootPet, VM *vm=NULL) const;
    // misc.
    static bool match(DistGrid *distgrid1, DistGrid *distgrid2, int *rc=NULL);
    int print() const;
    int validate() const;
    // serialize() and deserialize()
    int serialize(char *buffer, int *length, int *offset, ESMC_InquireFlag)
      const;
    static DistGrid *deserialize(char *buffer, int *offset);
    // connections
    static int connection(InterfaceInt *connection, int patchIndexA, 
      int patchIndexB, InterfaceInt *positionVector,
      InterfaceInt *orientationVector, InterfaceInt *repetitionVector);
  };  // class DistGrid

  
  class MultiDimIndexLoop{
   protected:
    vector<int> indexTupleStart;
    vector<int> indexTupleEnd;
    vector<int> indexTuple;
    vector<bool> skipMask;
   public:
    MultiDimIndexLoop(){
      indexTupleStart.resize(0);
      indexTupleEnd.resize(0);
      indexTuple.resize(0);
      skipMask.resize(0);
    }
    MultiDimIndexLoop(const vector<int> sizes){
      indexTupleEnd = sizes;
      indexTupleStart.resize(sizes.size());
      indexTuple.resize(sizes.size());
      skipMask.resize(sizes.size());
      for (int i=0; i<indexTuple.size(); i++){
        indexTupleStart[i] = indexTuple[i] = 0; // reset
        skipMask[i] = false;                    // reset
      }
    }
    MultiDimIndexLoop(const vector<int> offsets, const vector<int> sizes){
      indexTupleStart = offsets;
      indexTupleEnd = sizes;
      // todo: check that vector size matches, and throw exception if not
      indexTuple.resize(sizes.size());
      skipMask.resize(sizes.size());
      for (int i=0; i<indexTuple.size(); i++){
        indexTuple[i] = indexTupleStart[i];     // reset
        indexTupleEnd[i] += indexTupleStart[i]; // shift end by offsets
        skipMask[i] = false;                    // reset
      }
    }
    void setSkipDim(int dim){
      // todo: check that dim is between 0...,size-1
      skipMask[dim] = true;
    }
    void first(){
      for (int i=0; i<indexTuple.size(); i++)
        indexTuple[i] = indexTupleStart[i];  // reset
    }
    void last(){
      for (int i=0; i<indexTuple.size(); i++)
        indexTuple[i] = indexTupleEnd[i]-1;  // reset
    }
    void next(){
      if (skipMask[0])
        indexTuple[0] = indexTupleEnd[0]; // skip
      else
        ++indexTuple[0];                  // increment
      for (int i=0; i<indexTuple.size()-1; i++){
        if (indexTuple[i] == indexTupleEnd[i]){
          indexTuple[i] = indexTupleStart[i];  // reset
          if (skipMask[i+1])
            indexTuple[i+1] = indexTupleEnd[i+1]; // skip
          else
            ++indexTuple[i+1];                    // increment
        }
      }
    }
    bool isFirst(){
      for (int i=0; i<indexTuple.size(); i++)
        if (indexTuple[i] != indexTupleStart[i]) return false;
      return true;
    }
    bool isLast(){
      for (int i=0; i<indexTuple.size(); i++)
        if (indexTuple[i] != indexTupleEnd[i]-1) return false;
      return true;
    }
    bool isPastLast(){
      if (indexTuple[indexTuple.size()-1] < indexTupleEnd[indexTuple.size()-1])
        return false;
      return true;
    }
    const int *getIndexTuple(){
      return &indexTuple[0];
    }
    const int *getIndexTupleEnd(){
      return &indexTupleEnd[0];
    }
    const int *getIndexTupleStart(){
      return &indexTupleStart[0];
    }
  };  // class MultiDimIndexLoop

} // namespace ESMCI

#endif  // ESMCI_DistGrid_H

