
#include "geos-operator.h"
using namespace Rcpp;

class PointOnSurfaceOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSPointOnSurface_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
SEXP cpp_point_on_surface(SEXP dataLeft, SEXP ptype) {
  PointOnSurfaceOperator op;
  op.initProvider(dataLeft, ptype);
  return op.operate();
}

class CentroidOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSGetCentroid_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
SEXP cpp_centroid(SEXP dataLeft, SEXP ptype) {
  CentroidOperator op;
  op.initProvider(dataLeft, ptype);
  return op.operate();
}

class NodeOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSNode_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
SEXP cpp_node(SEXP dataLeft, SEXP ptype) {
  NodeOperator op;
  op.initProvider(dataLeft, ptype);
  return op.operate();
}

class BoundaryOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSBoundary_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
SEXP cpp_boundary(SEXP dataLeft, SEXP ptype) {
  BoundaryOperator op;
  op.initProvider(dataLeft, ptype);
  return op.operate();
}

class EnvelopeOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSEnvelope_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
SEXP cpp_envelope(SEXP dataLeft, SEXP ptype) {
  EnvelopeOperator op;
  op.initProvider(dataLeft, ptype);
  return op.operate();
}

class ConvexHullOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSConvexHull_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
SEXP cpp_convex_hull(SEXP dataLeft, SEXP ptype) {
  ConvexHullOperator op;
  op.initProvider(dataLeft, ptype);
  return op.operate();
}

class MinimumRotatedRectangleOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSMinimumRotatedRectangle_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
SEXP cpp_minimum_rotated_rectangle(SEXP dataLeft, SEXP ptype) {
  MinimumRotatedRectangleOperator op;
  op.initProvider(dataLeft, ptype);
  return op.operate();
}

class MinimumBoundingCircleOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    double radius;
    // it's unclear to me who is responsible for destroying this
    // geometry (probably me)
    GEOSGeometry* center;
    return GEOSMinimumBoundingCircle_r(this->context, geometry, &radius, &center);
  }
};

// [[Rcpp::export]]
SEXP cpp_minimum_bounding_circle(SEXP dataLeft, SEXP ptype) {
  MinimumBoundingCircleOperator op;
  op.initProvider(dataLeft, ptype);
  return op.operate();
}

class MinimumWidthOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSMinimumWidth_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
SEXP cpp_minimum_width(SEXP dataLeft, SEXP ptype) {
  MinimumWidthOperator op;
  op.initProvider(dataLeft, ptype);
  return op.operate();
}

class MinimumClearanceLineOperator: public UnaryGeometryOperator {
public:
  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    return GEOSMinimumClearanceLine_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
SEXP cpp_minimum_clearance_line(SEXP dataLeft, SEXP ptype) {
  MinimumClearanceLineOperator op;
  op.initProvider(dataLeft, ptype);
  return op.operate();
}

class MinimumClearanceOperator: public UnaryVectorOperator<NumericVector, double> {
public:
  double operateNext(GEOSGeometry* geometry) {
    double distance;
    GEOSMinimumClearance_r(this->context, geometry, &distance);
    return distance;
  }
};

// [[Rcpp::export]]
NumericVector cpp_minimum_clearance(SEXP dataLeft) {
  MinimumClearanceOperator op;
  op.initProvider(dataLeft);
  return op.operate();
}
