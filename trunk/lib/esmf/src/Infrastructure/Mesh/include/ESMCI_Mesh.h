//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_Mesh_h
#define ESMCI_Mesh_h

#include <Mesh/include/ESMCI_MeshDB.h>
#include <Mesh/include/ESMCI_CommRel.h>
#include <Mesh/include/ESMCI_FieldReg.h>
#include <Mesh/include/ESMCI_CommReg.h>

#include <mpi.h>

/**
 * @defgroup mesh
 * 
 * The basic system that is comprised of a topological mesh database,
 * field registrars, and communication.
 * 
 */

namespace ESMCI {
/**
 * Basic parallel mesh operations.  Aggregates the serial meshes,
 * the list of fields, and the parallel communiation relations.
 * 
 * @ingroup mesh
 */
class Mesh : public MeshDB, public FieldReg, public CommReg {
public:
friend void WriteExMesh(const MeshDB &mesh, const std::string &filename,int,double);
friend void WriteExMeshTimeStep(int,double,const MeshDB&,const std::string&);
friend void LoadExMesh(MeshDB &mesh, const std::string &filename,int);
friend void ReadMesh(Mesh&,const std::string&, bool,int);
Mesh();
~Mesh();

/**
 * Sets the mesh in 'stone'.  Builds the field data storage
 * and allocates space for fields.  All fields must be registered
 * prior to calling this function
 */
void Commit();

#if 0
 void GetImprintContexts(std::vector<UInt> nvalSet, std::vector<UInt> nvalSetObj);
#endif 

 void ProxyCommit(int numSetsArg,
		  std::vector<UInt> nvalSetSizesArg, std::vector<UInt> nvalSetValsArg,
		  std::vector<UInt> nvalSetObjSizesArg, std::vector<UInt> nvalSetObjValsArg);
 
 bool is_committed() const { return committed; }
 
 bool IsParallel() const { return true;}
 
/**
 * Objects are deleted on the local, serial mesh.  This function performs
 * a parallel resolution to see if the objects should be deleted, or if
 * they need to be kept for ghosting purposes
 */
void ResolvePendingDelete();

/**
 * Objects are created locally.  This function tries to see if the same object
 * has been created on neighboring processors, and resolves such creation.  Also,
 * if the object needs to be ghosted on a neighbor, this is done.  Global unique
 * identifiers are selected.
 */
void ResolvePendingCreate();

const CommRel &GetSymNodeRel() const;

void CreateGhost();
void RemoveGhost();

bool HasGhost() const { return sghost != NULL; }

CommReg &GhostComm() { ThrowRequire(sghost); return *sghost; }

// Create the sym rel
void build_sym_comm_rel(UInt obj_type);

void proxy_build_sym_comm_rel(UInt obj_type);

/*
 * When shared objects are marked to delete, we must find a new owner
 * for the shared object (on a proc that will keep it around)..  This 
 * function sets a new valid owner, or nproc if everyone is going away.
 * The spec is left unchanged.
 */
void resolve_cspec_delete_owners(UInt obj_type);

private:
void assign_new_ids();
CommReg *sghost;
bool committed;
};

} // namespace 

#endif
