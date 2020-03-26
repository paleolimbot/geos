
#include "geos-operator.h"
using namespace Rcpp;

class UnaryPredicateOperator: public UnaryVectorOperator<LogicalVector, bool> {
public:

  bool operateNext(GEOSGeometry* geometry)  {
    char result = this->operateNextGEOS(geometry);
    if (result == 2) {
      stop("Exception on binary predicate");
    } else if (result == 1) {
      return true;
    } else if (result == 0) {
      return  false;
    } else {
      stop("Unknown output from binary predicate");
    }
  }

  virtual char operateNextGEOS(GEOSGeometry* geometry) = 0;
};

class IsEmptyOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSisEmpty_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_empty(SEXP data) {
  IsEmptyOperator op;
  op.initProvider(data);
  return op.operate();
}

class IsSimpleOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSisSimple_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_simple(SEXP data) {
  IsSimpleOperator op;
  op.initProvider(data);
  return op.operate();
}

class HasZOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSHasZ_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_has_z(SEXP data) {
  HasZOperator op;
  op.initProvider(data);
  return op.operate();
}

class IsClosedOperator: public UnaryPredicateOperator {
  char operateNextGEOS(GEOSGeometry* geometry) {
    return GEOSisClosed_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
LogicalVector cpp_is_closed(SEXP data) {
  IsClosedOperator op;
  op.initProvider(data);
  return op.operate();
}

class GeomTypeIdOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGeomTypeId_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_geom_type_id(SEXP x) {
  GeomTypeIdOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetSRIDOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGetSRID_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_get_srid(SEXP x) {
  GetSRIDOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetNumGeometriesOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGetNumGeometries_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_n_geometries(SEXP x) {
  GetNumGeometriesOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetNumCoordinatesOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGetNumCoordinates_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_n_coordinates(SEXP x) {
  GetNumCoordinatesOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetNumPointsOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGeomGetNumPoints_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_n_points(SEXP x) {
  GetNumPointsOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetNumInteriorRingsOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGetNumInteriorRings_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_n_interior_rings(SEXP x) {
  GetNumInteriorRingsOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetDimensionsOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGeom_getDimensions_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_n_dimensions(SEXP x) {
  GetDimensionsOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetCoordinateDimensionsOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGeom_getCoordinateDimension_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_n_coordinate_dimensions(SEXP x) {
  GetCoordinateDimensionsOperator op;
  op.initProvider(x);
  return op.operate();
}
