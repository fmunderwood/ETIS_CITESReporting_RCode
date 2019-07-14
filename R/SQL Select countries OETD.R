# SQL code for selecting countries of origin, export, transit and destination
# For raw and worked ivory separately
# Ensures that transit countries are only listed if marked as having a seizure opportunity
#
# This file was written for multiple purposes
# Including the calculation of the Trade Chain Index
#==========================================================================
#
# Basic set of commands
#---------------------------------------------------------------------------
# Selects countries
#---------------------------------------------------------------------------
SQLstr.country <- 'SELECT id, code FROM public.countries ORDER BY id;'

#---------------------------------------------------------------------------
# Selects seizures and raw and worked weights 
#---------------------------------------------------------------------------
SQLstr.0 <- '
SELECT    id, status_id, seizure_year, discovered_country_id, 
raw_pieces, raw_weight, worked_pieces, worked_weight
FROM      public.seizures
WHERE     (raw_pieces>0 OR raw_weight>0 OR worked_pieces>0 OR worked_weight>0)
ORDER BY id;'

#---------------------------------------------------------------------------
# Selects all countries together and so gives lots of repeats
#---------------------------------------------------------------------------
SQLstr.all <- '
WITH  
tran_junc AS (
  SELECT  seizure_id, 
    CASE WHEN seizure_opportunity THEN country_id
    ELSE NULL
    END AS ct_id            -- set to NULL if no seizure opportunity
    FROM public.seizure_transit_countries ), 

tmp AS (
  SELECT    s.id, s.status_id, s.seizure_year, s.discovered_country_id, 
    s.raw_pieces, s.raw_weight, s.worked_pieces, s.worked_weight, 
    j1.country_id AS ct_orig_id, 
    j1r.country_id AS ct_raw_orig_id, 
    j1w.country_id AS ct_wkd_orig_id, 
    j2.country_id AS ct_expt_id, 
    j3.ct_id AS ct_tran_id, 
    s.destination_country_id
FROM      public.seizures s
LEFT JOIN      public.seizure_origin_countries j1 ON s.id = j1.seizure_id
LEFT JOIN      public.seizure_raw_origin_countries j1r ON s.id = j1r.seizure_id
LEFT JOIN      public.seizure_worked_origin_countries j1w ON s.id = j1w.seizure_id
LEFT JOIN      public.seizure_export_countries j2 ON s.id = j2.seizure_id
LEFT JOIN      tran_junc j3 ON s.id = j3.seizure_id
WHERE     s.raw_pieces>0 OR s.raw_weight>0 OR s.worked_pieces>0 OR s.worked_weight>0
ORDER BY  s.id)

--  remove country (set to NULL) if same as country of discovery ...
SELECT    id, status_id, seizure_year, 
raw_pieces, raw_weight, worked_pieces, worked_weight, 
NULLIF(ct_orig_id, discovered_country_id) AS ct_orig_mod, 
NULLIF(ct_raw_orig_id, discovered_country_id) AS ct_raw_orig_mod, 
NULLIF(ct_wkd_orig_id, discovered_country_id) AS ct_wkd_orig_mod, 
NULLIF(ct_expt_id, discovered_country_id) AS ct_expt_mod, 
NULLIF(ct_tran_id, discovered_country_id) AS ct_tran_mod, 
NULLIF(destination_country_id, discovered_country_id) AS ct_dest_mod
FROM      tmp
ORDER BY  id
;'



#---------------------------------------------------------------------------
# RAW 
#---------------------------------------------------------------------------
# Origin
SQLstr.r.orig <- '
WITH  
tmp AS (
  SELECT    s.id, s.status_id, s.seizure_year, s.discovered_country_id, 
    j1r.country_id AS ct_raw_orig_id
    FROM      public.seizures s
    LEFT JOIN      public.seizure_raw_origin_countries j1r ON s.id = j1r.seizure_id
    WHERE     s.raw_pieces>0 OR s.raw_weight>0
    ORDER BY  s.id)
--  remove country (set to NULL) if same as country of discovery ...
SELECT    id, status_id, seizure_year, 
NULLIF(ct_raw_orig_id, discovered_country_id) AS ct_raw_orig_mod
FROM      tmp
ORDER BY  id
;'


# Export
SQLstr.r.expt <- '
WITH  
tmp AS (
  SELECT  s.id, s.status_id, s.seizure_year, s.discovered_country_id, 
    j2.country_id AS ct_expt_id
    FROM      public.seizures s
    LEFT JOIN      public.seizure_export_countries j2 ON s.id = j2.seizure_id
    WHERE     s.raw_pieces>0 OR s.raw_weight>0
    ORDER BY  s.id)
--  remove country (set to NULL) if same as country of discovery ...
SELECT    id, status_id, seizure_year, 
NULLIF(ct_expt_id, discovered_country_id) AS ct_expt_mod
FROM      tmp
ORDER BY  id
;'

# Transit
SQLstr.r.tran <- '
WITH  
tran_junc AS (
  SELECT  seizure_id, 
    CASE WHEN seizure_opportunity THEN country_id
    ELSE NULL
    END AS ct_id            -- set to NULL if no seizure opportunity
    FROM public.seizure_transit_countries ), 

tmp AS (
  SELECT    s.id, s.status_id, s.seizure_year, s.discovered_country_id, 
    s.raw_pieces, s.raw_weight, s.worked_pieces, s.worked_weight, 
    j3.ct_id AS ct_tran_id
    FROM      public.seizures s
    LEFT JOIN      tran_junc j3 ON s.id = j3.seizure_id
    WHERE     s.raw_pieces>0 OR s.raw_weight>0
    ORDER BY  s.id)

--  remove country (set to NULL) if same as country of discovery ...
SELECT    id, status_id, seizure_year, 
NULLIF(ct_tran_id, discovered_country_id) AS ct_tran_mod 
FROM      tmp
ORDER BY  id
;'

# Destination
SQLstr.r.dest <- '
WITH  
tmp AS (
  SELECT    s.id, s.status_id, s.seizure_year, s.discovered_country_id, 
    s.destination_country_id
    FROM      public.seizures s
    WHERE     s.raw_pieces > 0 OR s.raw_weight > 0
    ORDER BY  s.id)
--  remove country (set to NULL) if same as country of discovery ...
SELECT    id, status_id, seizure_year, 
NULLIF(destination_country_id, discovered_country_id) AS ct_dest_mod
FROM      tmp
ORDER BY  id
;'

#---------------------------------------------------------------------------
# WORKED 
#---------------------------------------------------------------------------
# Origin
SQLstr.w.orig <- '
WITH  
tmp AS (
  SELECT    s.id, s.status_id, s.seizure_year, s.discovered_country_id, 
    j1w.country_id AS ct_worked_orig_id
    FROM      public.seizures s
    LEFT JOIN      public.seizure_worked_origin_countries j1w ON s.id = j1w.seizure_id
    WHERE     s.worked_pieces > 0 OR s.worked_weight > 0
    ORDER BY  s.id)
--  remove country (set to NULL) if same as country of discovery ...
SELECT    id, status_id, seizure_year, 
NULLIF(ct_worked_orig_id, discovered_country_id) AS ct_worked_orig_mod
FROM      tmp
ORDER BY  id
;'


# Export
SQLstr.w.expt <- '
WITH  
tmp AS (
  SELECT  s.id, s.status_id, s.seizure_year, s.discovered_country_id, 
    j2.country_id AS ct_expt_id
    FROM      public.seizures s
    LEFT JOIN      public.seizure_export_countries j2 ON s.id = j2.seizure_id
    WHERE     s.worked_pieces > 0 OR s.worked_weight > 0
    ORDER BY  s.id)
--  remove country (set to NULL) if same as country of discovery ...
SELECT    id, status_id, seizure_year, 
NULLIF(ct_expt_id, discovered_country_id) AS ct_expt_mod
FROM      tmp
ORDER BY  id
;'

# Transit
SQLstr.w.tran <- '
WITH  
tran_junc AS (
  SELECT  seizure_id, 
    CASE WHEN seizure_opportunity THEN country_id
    ELSE NULL
    END AS ct_id            -- set to NULL if no seizure opportunity
    FROM public.seizure_transit_countries ), 

tmp AS (
  SELECT    s.id, s.status_id, s.seizure_year, s.discovered_country_id, 
    j3.ct_id AS ct_tran_id
    FROM      public.seizures s
    LEFT JOIN      tran_junc j3 ON s.id = j3.seizure_id
    WHERE     s.worked_pieces>0 OR s.worked_weight>0
    ORDER BY  s.id)

--  remove country (set to NULL) if same as country of discovery ...
SELECT    id, status_id, seizure_year, 
NULLIF(ct_tran_id, discovered_country_id) AS ct_tran_mod 
FROM      tmp
ORDER BY  id
;'

# Destination
SQLstr.w.dest <- '
WITH  
tmp AS (
  SELECT    s.id, s.status_id, s.seizure_year, s.discovered_country_id, 
    s.destination_country_id
    FROM      public.seizures s
    WHERE     s.worked_pieces > 0 OR s.worked_weight > 0
    ORDER BY  s.id)
--  remove country (set to NULL) if same as country of discovery ...
SELECT    id, status_id, seizure_year, 
NULLIF(destination_country_id, discovered_country_id) AS ct_dest_mod
FROM      tmp
ORDER BY  id
;'

#=============================================================================
#=============================================================================
# As above but now does not exclude country if also country of discovery
#---------------------------------------------------------------------------
# RAW 
#---------------------------------------------------------------------------
# Origin
SQLstr.r.orig.d <- '
SELECT    s.id, s.status_id, s.seizure_year, 
j1r.country_id AS ct_raw_orig_id
FROM      public.seizures s
LEFT JOIN      public.seizure_raw_origin_countries j1r ON s.id = j1r.seizure_id
WHERE     s.raw_pieces>0 OR s.raw_weight>0
ORDER BY  s.id
;'


# Export
SQLstr.r.expt.d <- '
SELECT  s.id, s.status_id, s.seizure_year,
j2.country_id AS ct_expt_id
FROM      public.seizures s
LEFT JOIN      public.seizure_export_countries j2 ON s.id = j2.seizure_id
WHERE     s.raw_pieces>0 OR s.raw_weight>0
ORDER BY  s.id
;'

# Transit
SQLstr.r.tran.d <- '
WITH  
tran_junc AS (
SELECT  seizure_id, 
CASE WHEN seizure_opportunity THEN country_id
ELSE NULL
END AS ct_id            -- set to NULL if no seizure opportunity
FROM public.seizure_transit_countries ) 

SELECT    s.id, s.status_id, s.seizure_year, 
j3.ct_id AS ct_tran_id
FROM      public.seizures s
LEFT JOIN      tran_junc j3 ON s.id = j3.seizure_id
WHERE     s.raw_pieces > 0 OR s.raw_weight > 0
ORDER BY  s.id
;'

# Destination
SQLstr.r.dest.d <- '
SELECT    s.id, s.status_id, s.seizure_year, 
s.destination_country_id
FROM      public.seizures s
WHERE     s.raw_pieces > 0 OR s.raw_weight > 0
ORDER BY  s.id
;'

#---------------------------------------------------------------------------
# WORKED 
#---------------------------------------------------------------------------
# Origin
SQLstr.w.orig.d <- '
SELECT    s.id, s.status_id, s.seizure_year, 
j1w.country_id AS ct_worked_orig_id
FROM      public.seizures s
LEFT JOIN      public.seizure_worked_origin_countries j1w ON s.id = j1w.seizure_id
WHERE     s.worked_pieces > 0 OR s.worked_weight > 0
ORDER BY  s.id
;'


# Export
SQLstr.w.expt.d <- '
SELECT  s.id, s.status_id, s.seizure_year, 
j2.country_id AS ct_expt_id
FROM      public.seizures s
LEFT JOIN      public.seizure_export_countries j2 ON s.id = j2.seizure_id
WHERE     s.worked_pieces > 0 OR s.worked_weight > 0
ORDER BY  s.id
;'

# Transit
SQLstr.w.tran.d <- '
WITH  
tran_junc AS (
SELECT  seizure_id, 
CASE WHEN seizure_opportunity THEN country_id
ELSE NULL
END AS ct_id            -- set to NULL if no seizure opportunity
FROM public.seizure_transit_countries ) 

SELECT    s.id, s.status_id, s.seizure_year, 
j3.ct_id AS ct_tran_id
FROM      public.seizures s
LEFT JOIN      tran_junc j3 ON s.id = j3.seizure_id
WHERE     s.worked_pieces > 0 OR s.worked_weight > 0
ORDER BY  s.id

;'

# Destination
SQLstr.w.dest.d <- '
SELECT    s.id, s.status_id, s.seizure_year, 
s.destination_country_id
FROM      public.seizures s
WHERE     s.worked_pieces > 0 OR s.worked_weight > 0
ORDER BY  s.id

;'

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #

