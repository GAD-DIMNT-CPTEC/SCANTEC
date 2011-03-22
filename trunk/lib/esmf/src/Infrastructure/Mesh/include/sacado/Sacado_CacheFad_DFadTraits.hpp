// $Id: Sacado_CacheFad_DFadTraits.hpp,v 1.1 2007/08/07 20:45:56 dneckels Exp $ 
// $Source: /cvsroot/esmf/esmf/src/Infrastructure/Mesh/include/sacado/Sacado_CacheFad_DFadTraits.hpp,v $ 
// @HEADER
// ***********************************************************************
// 
//                           Sacado Package
//                 Copyright (2006) Sandia Corporation
// 
// Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
// the U.S. Government retains certain rights in this software.
// 
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//  
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//  
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// Questions? Contact David M. Gay (dmgay@sandia.gov) or Eric T. Phipps
// (etphipp@sandia.gov).
// 
// ***********************************************************************
//
// The forward-mode AD classes in Sacado are a derivative work of the
// expression template classes in the Fad package by Nicolas Di Cesare.  
// The following banner is included in the original Fad source code:
//
// ************ DO NOT REMOVE THIS BANNER ****************
//
//  Nicolas Di Cesare <Nicolas.Dicesare@ann.jussieu.fr>
//  http://www.ann.jussieu.fr/~dicesare
//
//            CEMRACS 98 : C++ courses, 
//         templates : new C++ techniques 
//            for scientific computing 
// 
//********************************************************
//
//  NumericalTraits class to illustrate TRAITS
//
//********************************************************
// @HEADER

#ifndef SACADO_CACHEFAD_DFADTRAITS_HPP
#define SACADO_CACHEFAD_DFADTRAITS_HPP

#include "Sacado_Traits.hpp"

// Forward declarations
namespace Sacado {
  namespace CacheFad {
    template <typename T1, typename T2> class DFad;
  }
}

namespace Sacado {

  //! Specialization of %Promote to DFad types
  template <typename ValueT, typename ScalarT>
  struct Promote< CacheFad::DFad<ValueT,ScalarT>, CacheFad::DFad<ValueT,ScalarT> > {
    typedef CacheFad::DFad<ValueT,ScalarT> type;
  };

  //! Specialization of %Promote to DFad types
  template <typename ValueT, typename ScalarT, typename R>
  struct Promote< CacheFad::DFad<ValueT,ScalarT>, R > {
    typedef typename ValueType< CacheFad::DFad<ValueT,ScalarT> >::type value_type_l;
    typedef typename ValueType<R>::type value_type_r;
    typedef typename Promote<value_type_l,value_type_r>::type value_type;

    typedef CacheFad::DFad<value_type,ScalarT> type;
  };

  //! Specialization of %Promote to DFad types
  template <typename L, typename ValueT, typename ScalarT>
  struct Promote< L, CacheFad::DFad<ValueT, ScalarT> > {
  public:

    typedef typename ValueType<L>::type value_type_l;
    typedef typename ValueType< CacheFad::DFad<ValueT,ScalarT> >::type value_type_r;
    typedef typename Promote<value_type_l,value_type_r>::type value_type;

    typedef CacheFad::DFad<value_type,ScalarT> type;
  };

  //! Specialization of %ScalarType to DFad types
  template <typename ValueT, typename ScalarT>
  struct ScalarType< CacheFad::DFad<ValueT,ScalarT> > {
    typedef ScalarT type;
  };

  //! Specialization of %ValueType to DFad types
  template <typename ValueT, typename ScalarT>
  struct ValueType< CacheFad::DFad<ValueT,ScalarT> > {
    typedef ValueT type;
  };

   //! Specialization of %ScalarValueType to DFad types
  template <typename ValueT, typename ScalarT>
  struct ScalarValueType< CacheFad::DFad<ValueT,ScalarT> > {
    typedef typename ScalarValueType< ValueT >::type type;
  };

  //! Specialization of %IsADType to DFad types
  template <typename ValueT, typename ScalarT>
  struct IsADType< CacheFad::DFad<ValueT,ScalarT> > {
    static const bool value = true;
  };

  //! Specialization of %IsADType to DFad types
  template <typename ValueT, typename ScalarT>
  struct IsScalarType< CacheFad::DFad<ValueT,ScalarT> > {
    static const bool value = false;
  };

  //! Specialization of %Value to DFad types
  template <typename ValueT, typename ScalarT>
  struct Value< CacheFad::DFad<ValueT,ScalarT> > {
    typedef typename ValueType< CacheFad::DFad<ValueT,ScalarT> >::type value_type;
    static const value_type& eval(const CacheFad::DFad<ValueT,ScalarT>& x) { 
      return x.val(); }
  };

} // namespace Sacado

#endif // SACADO_FAD_DFADTRAITS_HPP
