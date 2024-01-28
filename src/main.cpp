// Copyright 2024 Petros Koutsolampros
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

#include "salalib/shapemap.h"
#include "salalib/shapegraph.h"
#include "salalib/mgraph.h"
#include "salalib/mapconverter.h"
#include "genlib/p2dpoly.h"

#include <Rcpp.h>
#include <typeinfo>
#include <exception>

// debian 12 install:
// sudo apt install libudunits2-dev libproj-dev libgdal-dev

// [[Rcpp::plugins(cpp17)]]

RCPP_EXPOSED_CLASS(ShapeMap);
RCPP_EXPOSED_CLASS(ShapeGraph);

// Simple class meant to hold only the data of the MetaGraph
struct MetaGraphData {
    std::vector<Rcpp::XPtr<ShapeMap>> shapeMaps;
    std::vector<Rcpp::XPtr<ShapeGraph>> shapeGraphs;
};

RCPP_EXPOSED_CLASS(MetaGraphData);

//*// [[Rcpp::export("getMetaGraph")]]
MetaGraphData readMetaGraph(std::string fileName) {
    Rcpp::Rcerr << "Loading MetaGraph at: " << fileName << std::endl;
    auto m = std::unique_ptr<MetaGraph>(new MetaGraph(fileName));
    m->readFromFile(fileName);
    Rcpp::Rcerr << "- bb: " << m->getBoundingBox().area() << std::endl;

    MetaGraphData mgraphData;
    for (auto & drawingFile : m->m_drawingFiles) {
        Rcpp::Rcerr << " - drawingName: "
                    << drawingFile.getName() << std::endl;
        for (auto &shapeMap : drawingFile.m_spacePixels) {
            Rcpp::Rcerr << " - shapeMapName: "
                        << shapeMap.getName() << std::endl;
            ShapeMap m(std::move(shapeMap));
            mgraphData.shapeMaps.push_back(Rcpp::XPtr<ShapeMap>(&m, true));
        }
    }
    return mgraphData;
}

namespace ShapeMapFuncs {
Rcpp::XPtr<ShapeMap> make(std::string name) {
    return Rcpp::XPtr<ShapeMap>(new ShapeMap(name), true);
}

std::string getName(Rcpp::XPtr<ShapeMap> shapeMap) {
    return shapeMap->getName();
}
std::vector<std::string> getAttributeNames(Rcpp::XPtr<ShapeMap> shapeMap) {
    std::vector<std::string> names;
    auto &attributes = shapeMap->getAttributeTable();
    int numCols = attributes.getNumColumns();
    // + 1 for the key column
    names.reserve(1 + numCols);
    names.push_back(attributes.getColumnName(size_t(-1)));
    for(int i = 0; i < attributes.getNumColumns(); ++i) {
        names.push_back(attributes.getColumnName(i));
    }
    return names;
}

std::map<std::string, std::vector<double>> getAttributeData(
        Rcpp::XPtr<ShapeMap> shapeMap,
        std::vector<std::string> attributeNames) {
    auto &attrbs = shapeMap->getAttributeTable();
    std::map<std::string, std::vector<double>> data;
    for (auto &attributeName: attributeNames) {
        auto& attributeData = data[attributeName];
        attributeData.reserve(attrbs.getNumRows());
        if (attributeName == attrbs.getColumnName(size_t(-1))) {
            for(auto rowIt = attrbs.begin(); rowIt != attrbs.end(); ++rowIt) {
                attributeData.push_back(rowIt->getKey().value);
            }
        } else {
            size_t colIdx = attrbs.getColumnIndex(attributeName);
            for(auto rowIt = attrbs.begin(); rowIt != attrbs.end(); ++rowIt) {
                attributeData.push_back(rowIt->getRow().getValue(colIdx));
            }
        }
    }
    return data;
}
}

// sample function showing how to extract data from a dataframe
void exploreDF(Rcpp::DataFrame &df) {

    for (auto it = df.begin(); it != df.end(); it++) {
        // https://gallery.rcpp.org/articles/rcpp-wrap-and-recurse/
        // #define INTSXP      13    /* integer vectors */
        // #define REALSXP     14    /* real variables */
        // #define CPLXSXP     15    /* complex variables */
        // #define STRSXP      16    /* string vectors */
        // #define DOTSXP      17    /* dot-dot-dot object */
        // #define ANYSXP      18    /* make "any" args work. */
        // #define VECSXP      19    /* generic vectors */
        switch( TYPEOF(*it) ) {
        case REALSXP: {
            auto tmp = Rcpp::as<Rcpp::NumericVector>(*it);

            // std::cout << "REALSXP" << std::endl;
            // for (auto attrName : tmp.attributeNames()) {
            //   std::cout << attrName << ": " << std::endl;
            //   //Rcpp::print(tmp.attr(attrName));
            // }
            break;
        }
        case STRSXP: {
            auto tmp = Rcpp::as<Rcpp::StringVector>(*it);

            // std::cout << "REALSXP" << std::endl;
            // for (auto attrName : tmp.attributeNames()) {
            //   std::cout << attrName << ": " << std::endl;
            //   //Rcpp::print(tmp.attr(attrName));
            // }
            break;
        }
        case INTSXP: {
            if( Rf_isFactor(*it) ) break; // factors have internal type INTSXP
            auto tmp = Rcpp::as<Rcpp::IntegerVector>(*it);

            // std::cout << "INTSXP" << std::endl;
            // print(tmp);
            // for (auto attrName : tmp.attributeNames()) {
            //   std::cout << attrName << ": " << std::endl;
            //   //Rcpp::print(tmp.attr(attrName));
            // }
            break;
        }
        case VECSXP: {
            // generic vector
            auto gv = Rcpp::as<Rcpp::GenericVector>(*it);
            if (gv.hasAttribute("class") &&
                TYPEOF(gv.attr("class")) == STRSXP) {
                // has a class attribute which is a string vector
                auto classData = Rcpp::as<Rcpp::StringVector>(gv.attr("class"));
                bool isLineString = std::find(
                    classData.begin(), classData.end(),
                    "sfc_LINESTRING") != classData.end();

                if (isLineString) {
                    // do stuff with line vector
                }
            }
            break;
        }
        default: {
            Rcpp::stop("incompatible SEXP encountered; only accepts lists"
                           " with REALSXPs, STRSXPs, VECSXPs and INTSXPs");
        }
        }
    }
}

bool hasClass(Rcpp::DataFrame &df, std::string cl) {
    if (df.hasAttribute("class") &&
        TYPEOF(df.attr("class")) == STRSXP) {
        // has a class attribute which is a string vector
        auto classData = Rcpp::as<Rcpp::StringVector>(df.attr("class"));
        return std::find(
            classData.begin(), classData.end(),
            cl) != classData.end();
    }
    return false;
}

Rcpp::StringVector getStringVectorAttr(Rcpp::DataFrame &df, std::string cl) {

    if (!df.hasAttribute(cl)) {
        Rcpp::stop("Dataframe does not have the attribute \"" + cl + "\"");
    }
    return Rcpp::as<Rcpp::StringVector>(df.attr(cl));
}

int getGeometryColumnIndex(Rcpp::DataFrame &df) {
    return df.findName(Rcpp::as<std::string>(
            *getStringVectorAttr(df, "sf_column").begin()));
}

Rcpp::XPtr<ShapeMap> toShapeMap(
        Rcpp::DataFrame &df) {

    if (!hasClass(df, "sf")) {
        Rcpp::stop("Not an sf dataframe");
    }

    if (TYPEOF(df.attr("row.names")) != INTSXP) {
        Rcpp::stop("Non-integer row.names are not supported");
    }

    const auto &rowNames = Rcpp::as<Rcpp::IntegerVector>(df.attr("row.names"));

    auto geometryColumnIdx = getGeometryColumnIndex(df);
    const auto &lines = Rcpp::as<Rcpp::GenericVector>(df.at(geometryColumnIdx));

    Rcpp::XPtr<ShapeMap> shp(new ShapeMap("tmp_df_shp"));
    int rowNameColIdx = shp->addAttribute("df_row_name");

    if (rowNameColIdx == -1) {
        // error adding column (e.g., duplicate column names)
        Rcpp::stop("Error creating df row column");
    }

    auto lit = lines.begin();
    for (auto rit = rowNames.begin(); rit != rowNames.end(); ++rit) {
        int rowName = *rit;
        auto coords = Rcpp::as<Rcpp::NumericVector>(*lit);

        if (coords.size() == 4) {
            // 2D line x1,y1,x2,y2
            std::map<int, float> extraAttributes;
            extraAttributes.emplace(rowNameColIdx, rowName);

            Line line(Point2f(coords[0], coords[1]),
                      Point2f(coords[2], coords[3]));
            shp->makeLineShape(
                    line,
                    false /* through_ui */,
                    false /* tempshape */,
                    extraAttributes);
        }
        ++lit;
    }


    return shp;
}


Rcpp::XPtr<ShapeGraph> toAxialShapeGraph(Rcpp::XPtr<ShapeMap> shapeMap) {
    std::unique_ptr<ShapeGraph> axMap(MapConverter::convertDataToAxial(
            nullptr, "ax_map", *(shapeMap.get()), true));

    // release the unique_ptr so that it's not deleted on scope close
    ShapeGraph *shpgp = axMap.release();

    return Rcpp::XPtr<ShapeGraph>(shpgp);
}



// should not expose metagraph, instead only shapemaps/shapegraphs
// and use the metagraph just to import the shapemaps to an R list
RCPP_MODULE(aedon_module) {
    Rcpp::class_<MetaGraphData>("MetaGraphData")
    .field_readonly("shapeMaps", &MetaGraphData::shapeMaps)
    .field_readonly("shapeGraphs", &MetaGraphData::shapeGraphs)
    ;

    // Rcpp::class_<ShapeMap>("sala_ShapeMap")
    // .constructor<std::string, int>()
    // .method("getName", &ShapeMap::getName)
    // ;
    Rcpp::function("readMetaGraph", &readMetaGraph);
    Rcpp::function("getName", &ShapeMapFuncs::getName);
    Rcpp::function("makeShapeMap", &ShapeMapFuncs::make);
    Rcpp::function("getAttributeNames", &ShapeMapFuncs::getAttributeNames);
    Rcpp::function("getAttributeData", &ShapeMapFuncs::getAttributeData);


    Rcpp::function("toShapeMap", &toShapeMap);
    Rcpp::function("toAxialShapeGraph", &toAxialShapeGraph);
}


