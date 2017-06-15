SET DATEFIRST 1

select
	[Date] = cast(pot.[MsgReportDate] as date),
	[Hour] = datepart(hour, pot.[MsgReportDate]),
	[DayOfWeek] = case 
		when sc.OperatingDayTypeKey = 1 then datepart(WEEKDAY, cast(pot.[MsgReportDate] as date)) 
		when sc.OperatingDayTypeKey = 2 then 6
		when sc.OperatingDayTypeKey = 3 then 7
	end,
	[CheckInCount] = count(1)
from
	[data].[RK_PartOfTrip] pot (nolock)
	join [data].[RT_Journey] j (nolock) on j.[JourneyRef] = pot.[JourneyRef]
	join [dim].[Date] d on d.[Date] = cast(pot.[MsgReportDate] as date)
	join [fact].[PAX_ServiceCalendar] sc on sc.OperatingDayDateKey = d.[DateKey]
where
	[PartialTripToStopPointID] is not null
	and pot.[MsgReportDate] between '2016-10-01 00:00:00' and '2017-03-31 23:59:59'
	and j.[LineNumber] = 5
group by
	cast(pot.[MsgReportDate] as date),
	sc.OperatingDayTypeKey,
	datepart(hour, pot.[MsgReportDate])
order by
	cast(pot.[MsgReportDate] as date),
	datepart(hour, pot.[MsgReportDate])