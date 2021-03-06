set datefirst 1;

select
	[Date] = cast(p.[PlannedArrivalDateTime] as date),
	[Hour] = datepart(hour, p.[PlannedArrivalDateTime]),
	[DayOfWeek] = case 
		when sc.OperatingDayTypeKey = 1 then datepart(WEEKDAY, cast(p.[PlannedArrivalDateTime] as date)) 
		when sc.OperatingDayTypeKey = 2 then 6
		when sc.OperatingDayTypeKey = 3 then 7
	end,
	[CheckInCount] = sum(p.[TotalCheckInCount])
from
	[data].[RT_Journey] j (nolock)
	join [data].[RT_JourneyPoint] p (nolock) on p.[JourneyRef] = j.[JourneyRef]
	join [dim].[Date] d on d.[Date] = cast(p.[PlannedArrivalDateTime] as date)
	join [fact].[PAX_ServiceCalendar] sc on sc.OperatingDayDateKey = d.[DateKey]
where
	j.[OperatingDayDate] between '2016-09-30' and '2017-03-31'
	and cast(p.[PlannedArrivalDateTime] as date) between '2016-10-01' and '2017-03-31'
	and j.[LineNumber] = 5
group by
	cast(p.[PlannedArrivalDateTime] as date),
	sc.OperatingDayTypeKey,
	datepart(hour, p.[PlannedArrivalDateTime])
order by
	[Date],
	[Hour]