/****** Script for SelectTopNRows command from SSMS  ******/

DECLARE @Sta1 Table (LakeID varchar(20), [Year] int, SprTP float)
DECLARE @Sta2 Table (LakeID varchar(20), [Year] int, SprTP float)
DECLARE @Sta1Sta2 Table  (LakeID varchar(20), [Year] int, Sta1_SprTP float, Sta2_SprTP float)

INSERT INTO @Sta1
SELECT
	LakeID,
	[Year],
	SprTP
	FROM [dbo].[SprData_AnnualMeans_StaAll]
	WHERE (LakeID<>'Memphremagog' AND LakeStationNo='1') OR (LakeID='Memphremagog' AND LakeStationNo='4')

	--SELECT * FROM @Sta1

INSERT INTO @Sta2
SELECT
	LakeID,
	[Year],
	SprTP
	FROM [dbo].[SprData_AnnualMeans_StaAll]
	WHERE (LakeID<>'Memphremagog' AND LakeStationNo='2') OR (LakeID='Memphremagog' AND LakeStationNo='3')

INSERT INTO @Sta1Sta2
SELECT 
 s1.LakeID,
 s1.[Year],
 s1.SprTP as 'Sta1_SprTP',
 s2.SprTP as 'Sta2_SprTP'
 FROM @Sta1 s1 
 JOIN @Sta2 s2 ON s1.LakeID=s2.LakeID and s1.[Year] = s2.[Year]
 ORDER BY s1.LakeID, s1.[Year];

 ;with cte  (LakeID, N) as 
 (SELECT LakeID, count(LakeID) FROM @Sta1Sta2 GROUP BY LakeID)
 SELECT 
 s.LakeID
 ,s.[Year]
 ,s.Sta1_SprTP
 ,s.Sta2_SprTP
  ,c.N
 FROM @Sta1Sta2 s
 INNER JOIN cte c on s.LakeID=c.LakeID
Where c.N >= 5
 Order by LakeID, [Year]
  
 