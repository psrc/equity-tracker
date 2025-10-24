SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE OR ALTER VIEW [equity].[v_tract_shares] AS (
SELECT b.geoid
      ,b.data_year
      ,b.county
      ,CASE b.poc_quintile        WHEN 1.0 THEN 'Low' WHEN 1.5 THEN 'Low - Low Medium' WHEN 2.0 THEN 'Low Medium' WHEN 2.5 THEN 'Low Medium - Medium' WHEN 3.0 THEN 'Medium' WHEN 4.0 THEN 'Medium High' WHEN 5.0 THEN 'High' END AS poc_quintile
      ,CASE b.income_quintile     WHEN 1.0 THEN 'Low' WHEN 1.5 THEN 'Low - Low Medium' WHEN 2.0 THEN 'Low Medium' WHEN 2.5 THEN 'Low Medium - Medium' WHEN 3.0 THEN 'Medium' WHEN 4.0 THEN 'Medium High' WHEN 5.0 THEN 'High' END AS income_quintile
      ,CASE b.disability_quintile WHEN 1.0 THEN 'Low' WHEN 1.5 THEN 'Low - Low Medium' WHEN 2.0 THEN 'Low Medium' WHEN 2.5 THEN 'Low Medium - Medium' WHEN 3.0 THEN 'Medium' WHEN 4.0 THEN 'Medium High' WHEN 5.0 THEN 'High' END AS disability_quintile
      ,CASE b.youth_quintile      WHEN 1.0 THEN 'Low' WHEN 1.5 THEN 'Low - Low Medium' WHEN 2.0 THEN 'Low Medium' WHEN 2.5 THEN 'Low Medium - Medium' WHEN 3.0 THEN 'Medium' WHEN 4.0 THEN 'Medium High' WHEN 5.0 THEN 'High' END AS youth_quintile
      ,CASE b.older_quintile      WHEN 1.0 THEN 'Low' WHEN 1.5 THEN 'Low - Low Medium' WHEN 2.0 THEN 'Low Medium' WHEN 2.5 THEN 'Low Medium - Medium' WHEN 3.0 THEN 'Medium' WHEN 4.0 THEN 'Medium High' WHEN 5.0 THEN 'High' END AS older_quintile
      ,CASE b.lep_quintile        WHEN 1.0 THEN 'Low' WHEN 1.5 THEN 'Low - Low Medium' WHEN 2.0 THEN 'Low Medium' WHEN 2.5 THEN 'Low Medium - Medium' WHEN 3.0 THEN 'Medium' WHEN 4.0 THEN 'Medium High' WHEN 5.0 THEN 'High' END AS lep_quintile
  FROM Elmer.equity.tract_shares AS b);
GO

