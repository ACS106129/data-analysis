# %%
from os import makedirs
from typing import Union

from matplotlib.pyplot import subplots
from pandas import read_csv
from pandas.core.frame import DataFrame
from pylab import mpl


def get_club_df(df: DataFrame, group_list: list[str], club_kind: str) -> DataFrame:
    return df[df[club_kind] == 'Y'].groupby(group_list, as_index=False).agg(**{club_kind: ('average', 'mean')})


def get_line_plot_kws(y: Union[list[str], str], ylabel: str = '平均成績'):
    return {'kind': 'line', 'x': 'semester', 'xlabel': '學期',
            'y': y, 'ylabel': ylabel, 'marker': '.'}


def get_ax_by_group(df: DataFrame, group_list: Union[list[str], str], title: str, get_plot_kws_func, *args, **kwargs):
    _, ax = subplots()
    keys = []
    for key, grp in df.groupby(group_list):
        if type(group_list) is list:
            key = key[-1]
            if key in keys:
                continue
            keys.append(key)
        ax = grp.plot(ax=ax, title=title, label=key, **
                      get_plot_kws_func(*args, **kwargs))
    return ax


def get_percent_col(name: str, nume: DataFrame, deno: DataFrame) -> dict[str, DataFrame]:
    return {name: ((1 - nume / deno) * 100).round()}


def get_sgrade_df(df: DataFrame) -> DataFrame:
    return df.assign(**{**get_percent_col('class_pr', df[PERCENT_ELEMENTS[0]], df[PERCENT_ELEMENTS[1]]),
                        **get_percent_col('dept_pr', df[PERCENT_ELEMENTS[2]], df[PERCENT_ELEMENTS[3]])}).drop(
        columns=PERCENT_ELEMENTS)


def read_csv_as_lowercase(file: str):
    csv = read_csv(file)
    csv.columns = csv.columns.str.lower()
    return csv


mpl.rcParams['font.sans-serif'] = ['Microsoft YaHei']
mpl.rcParams['axes.unicode_minus'] = False
mpl.rcParams["figure.figsize"] = [13, 7]
mpl.rcParams["figure.autolayout"] = True
CLUB_KINDS = ['系學會社團', '學術型社團', '康樂性社團', '聯誼性社團', '服務性社團']
FILENAMES = [f'{x}({y})' for x in ['院成績', '綜合成績']
             for y in ['有無參與社團', '系學會與否', '社團性質分群', '參加系學會']]
SEMESTERS = [1021, 1022, 1031, 1032, 1041, 1042, 1051, 1052]
PERCENT_ELEMENTS = ['a_no', 'a_sum', 'dept_no', 'dept_sum']
STUDENT_CLUB_DF = read_csv_as_lowercase('res/學生社團資料.csv')
STUDENT_GRADE_DF = read_csv_as_lowercase('res/學生成績資料.csv')
STUDENT_IDENTITY_DF = read_csv_as_lowercase('res/個別學生資料.csv')

# make sure all student id is valid from 1021 - 1052 on grade comparison, and prevent zero grade
available_sidf = STUDENT_IDENTITY_DF[STUDENT_IDENTITY_DF['離校學期']
                                     >= SEMESTERS[0]]
__sid_ggroup = STUDENT_GRADE_DF.groupby('s_id')
available_sgdf = STUDENT_GRADE_DF[__sid_ggroup['semester'].transform(lambda x: set(SEMESTERS) == set(
    x)) & __sid_ggroup['average'].transform(lambda x: 0 not in list(x))].sort_values(['s_id', 'semester'])
available_scdf = STUDENT_CLUB_DF[STUDENT_CLUB_DF['s_id'].isin(available_sgdf['s_id']) & STUDENT_CLUB_DF['semester'].isin(
    SEMESTERS) & STUDENT_CLUB_DF['s_id'].isin(available_sidf[available_sidf['參加社團'] == 'Y']['s_id'])]

# student grade dataframe
sgrade_df = get_sgrade_df(available_sgdf).merge(available_sidf[['s_id', '學院', '科系', '參加社團', *CLUB_KINDS]], on='s_id', how='inner').merge(
    available_scdf[['s_id', 'club_id', 'assistant_manager_flag', 'manager_flag']], on='s_id', how='left').drop_duplicates(
        ['s_id', 'semester']).reset_index(drop=True)
sgrade_df.to_csv('output/student_grade.csv')

__club_separator = sgrade_df.groupby(
    '學院')['參加社團'].transform(lambda x: len(set(x)) == 2)

dept_is_join_df = sgrade_df[__club_separator].groupby(
    ['學院', 'semester', '參加社團'], as_index=False).agg({'average': 'mean'})
dept_is_join_df['diff'] = dept_is_join_df.groupby(
    ['學院', 'semester'])['average'].diff().fillna(method='backfill')
dept_dClub_is_join_df = sgrade_df[__club_separator].groupby(
    ['學院', 'semester', '系學會社團'], as_index=False).agg({'average': 'mean'})
dept_dClub_is_join_df['diff'] = dept_dClub_is_join_df.groupby(
    ['學院', 'semester'])['average'].diff().fillna(method='backfill')
dept_dClub_joined_df = sgrade_df[sgrade_df['系學會社團'] == 'Y'].groupby(
    ['學院', 'semester'], as_index=False).agg({'average': 'mean'})
dept_club_kinds_df = get_club_df(sgrade_df, ['學院', 'semester'], CLUB_KINDS[0])
gen_is_join_df = sgrade_df[__club_separator].groupby(
    ['semester', '參加社團'], as_index=False).agg({'average': 'mean'})
gen_dClub_is_join_df = sgrade_df[__club_separator].groupby(
    ['semester', '系學會社團'], as_index=False).agg({'average': 'mean'})
gen_dClub_join_df = sgrade_df[sgrade_df['系學會社團'] == 'Y'].groupby(
    ['semester'], as_index=False).agg({'average': 'mean'})
gen_club_kinds_df = get_club_df(sgrade_df, ['semester'], CLUB_KINDS[0])
for kind in CLUB_KINDS[1:]:
    dept_club_kinds_df = dept_club_kinds_df.merge(get_club_df(
        sgrade_df, ['學院', 'semester'], kind), on=['學院', 'semester'], how='left')
    gen_club_kinds_df = gen_club_kinds_df.merge(
        get_club_df(sgrade_df, ['semester'], kind), on=['semester'], how='left')

##### export xlsx/csv below #####
makedirs('output/csv', exist_ok=True)
makedirs('output/xlsx', exist_ok=True)
for i, e in enumerate([dept_is_join_df, dept_dClub_is_join_df, dept_club_kinds_df, dept_dClub_joined_df,
                       gen_is_join_df, gen_dClub_is_join_df, gen_club_kinds_df, gen_dClub_join_df]):
    e.to_excel(f'output/xlsx/{FILENAMES[i]}.xlsx')
    e.to_csv(f'output/csv/{FILENAMES[i]}.csv')
    e['semester'] = e['semester'].astype(str)

##### plotting #####
LEGEND = {'bbox_to_anchor': (1, 1), 'loc': 'upper left'}

dept_is_join_ax = get_ax_by_group(
    dept_is_join_df, ['參加社團', '學院'], FILENAMES[0], get_line_plot_kws, 'diff', '參加社團正差')
dept_is_join_ax.legend(**LEGEND)

dept_dClub_is_join_ax = get_ax_by_group(
    dept_dClub_is_join_df, ['系學會社團', '學院'], FILENAMES[1], get_line_plot_kws, 'diff', '系學會社團正差')
dept_dClub_is_join_ax.legend(**LEGEND)

dept_club_kinds_ax_list = []
for key, grp in dept_club_kinds_df.groupby('學院'):
    grp = grp.dropna(axis=1)
    ax = grp.plot(
        title=f'{FILENAMES[2]}-{key}', **get_line_plot_kws(grp.columns[2:]))
    ax.legend(**LEGEND)
    dept_club_kinds_ax_list.append(ax)

dept_dClub_joined_ax = get_ax_by_group(
    dept_dClub_joined_df, '學院', FILENAMES[3], get_line_plot_kws, 'average')
dept_dClub_joined_ax.legend(**LEGEND)

gen_is_join_ax = get_ax_by_group(
    gen_is_join_df, '參加社團', FILENAMES[4], get_line_plot_kws, 'average')
gen_is_join_ax.legend(['無', '參加社團'], **LEGEND)

gen_dClub_is_join_ax = get_ax_by_group(
    gen_dClub_is_join_df, '系學會社團', FILENAMES[5], get_line_plot_kws, 'average')
gen_dClub_is_join_ax.legend(['其他', '系學會社團'], **LEGEND)

gen_club_kinds_ax = gen_club_kinds_df.plot(
    title=FILENAMES[6], **get_line_plot_kws(CLUB_KINDS))
gen_club_kinds_ax.legend(**LEGEND)

gen_dClub_joined_ax = gen_dClub_join_df.plot(
    title=FILENAMES[7], **get_line_plot_kws('average'))
gen_dClub_joined_ax.legend(['綜合'], **LEGEND)

##### export to jpg #####
makedirs('output/img', exist_ok=True)
for e in [dept_is_join_ax, dept_dClub_is_join_ax, dept_dClub_joined_ax, gen_is_join_ax,
          gen_dClub_is_join_ax, gen_club_kinds_ax, gen_dClub_joined_ax, *dept_club_kinds_ax_list]:
    e.get_figure().savefig(f'output/img/{e.get_title()}.jpg')
# %%
