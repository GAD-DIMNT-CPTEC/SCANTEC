// $Id: Sacado_Tay_CacheTaylorTraits.hpp,v 1.1 2007/08/07 20:45:58 dneckels Exp $ 
// $Source: /cvsroot/esmf/esmf/src/Infrastructure/Mesh/include/sacado/Sacado_Tay_CacheTaylorTraits.hpp,v $ 
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
// @HEADER

#ifndef SACADO_TAYLOR_DTAYLORTRAITS_HPP
#define SACADO_TAYLOR_DTAYLORTRAITS_HPP

#include "Sacado_Traits.hpp"

// Forward declarations
namespace Sacado {
  namespace Tay {
    template <typename T> class CacheTaylor;
  }
}

namespace Sacado {

  //! Specialization of %Promote to CacheTaylor types
  template <typename T>
  class Promote< Tay::CacheTaylor<T>, Tay::CacheTaylor<T> > {
  public:

    typedef Tay::CacheTaylor<T> type;
  };

  //! Specialization of %Promote to CacheTaylor types
  template <typename L, typename R>
  class Promote< Tay::CacheTaylor<L>, R > {
  public:

    typedef typename ValueType< Tay::CacheTaylor<L> >::type value_type_l;
    typedef typename ValueType<R>::type value_type_r;
    typedef typename Promote<value_type_l,value_type_r>::type value_type;

    typedef Tay::CacheTaylor<value_type> type;
  };

  //! Specialization of %Promote to CacheTaylor types
  template <typename L, typename R>
  class Promote< L, Tay::CacheTaylor<R> > {
  public:

    typedef typename ValueType<L>::type value_type_l;
    typedef typename ValueType< Tay::CacheTaylor<R> >::type value_type_r;
    typedef typename Promote<value_type_l,value_type_r>::type value_type;

    typedef Tay::CacheTaylor<value_type> type;
  };

  //! Specialization of %ScalarType to DFad types
  template <typename T>
  struct ScalarType< Tay::CacheTaylor<T> > {
    typedef T type;
  };

  //! Specialization of %ValueType to DFad types
  template <typename T>
  struct ValueType< Tay::CacheTaylor<T> > {
    typedef T type;
  };

   //! Specialization of %ScalarValueType to DFad types
  template <typename T>
  struct ScalarValueType< Tay::CacheTaylor<T> > {
    typedef typename ScalarValueType< T >::type type;
  };

  //! Specialization of %IsADType to DFad types
  template <typename T>
  struct IsADType< Tay::CacheTaylor<T> > {
    static const bool value = true;
  };

  //! Specialization of %IsADType to DFad types
  template <typename T>
  struct IsScalarType< Tay::CacheTaylor<T> > {
    static const bool value = false;
  };

  //! Specialization of %Value to DFad types
  template <typename T>
  struct Value< Tay::CacheTaylor<T> > {
    typedef typename ValueType< Tay::CacheTaylor<T> >::type value_type;
    static const value_type& eval(const Tay::CacheTaylor<T>& x) { 
      return x.val(); }
  };

} // namespace Sacado

#endif // SACADO_TAYLOR_DTAYLORTRAITS_HPP
